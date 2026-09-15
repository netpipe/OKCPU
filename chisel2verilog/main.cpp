#include <QApplication>
#include <QMainWindow>
#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>
#include <QFileDialog>
#include <QTextEdit>
#include <QLabel>
#include <QProcess>
#include <QDir>
#include <QFile>
#include <QMessageBox>
#include <QTemporaryDir>
#include <QFileInfo>
#include <QLineEdit>
#include <QDirIterator>
#include <QObject>
#include <QTextStream>

class ChiselConverter : public QMainWindow {
    Q_OBJECT

public:
    ChiselConverter(QWidget *parent = nullptr) : QMainWindow(parent) {
        setWindowTitle("Chisel to Verilog Converter");
        resize(700, 500);

        QWidget *centralWidget = new QWidget(this);
        setCentralWidget(centralWidget);
        QVBoxLayout *layout = new QVBoxLayout(centralWidget);

        inputLabel = new QLabel("Input Scala File: Not selected");
        QPushButton *selectInputBtn = new QPushButton("Select Input .scala File");
        connect(selectInputBtn, &QPushButton::clicked, this, &ChiselConverter::selectInput);

        outputLabel = new QLabel("Output Directory: Not selected");
        QPushButton *selectOutputBtn = new QPushButton("Select Output Directory");
        connect(selectOutputBtn, &QPushButton::clicked, this, &ChiselConverter::selectOutput);

        QLabel *mainClassLabel = new QLabel("Main Object Name (default: Main):");
        mainClassEdit = new QLineEdit("Main");

        convertBtn = new QPushButton("Convert to Verilog");
        convertBtn->setStyleSheet("background-color: #4CAF50; color: white; font-weight: bold; padding: 10px;");
        connect(convertBtn, &QPushButton::clicked, this, &ChiselConverter::convert);

        logText = new QTextEdit();
        logText->setReadOnly(true);
        logText->setStyleSheet("background-color: #2b2b2b; color: #a9b7c6; font-family: monospace;");
        logText->append("Welcome! Ensure `sbt` and `firtool` are installed in your system PATH.");
        logText->append("1. Select the Chisel source file (e.g., NANOS-CPU2.scala).");
        logText->append("2. Select an output directory.");
        logText->append("3. Click Convert.");

        layout->addWidget(inputLabel);
        layout->addWidget(selectInputBtn);
        layout->addWidget(outputLabel);
        layout->addWidget(selectOutputBtn);
        layout->addWidget(mainClassLabel);
        layout->addWidget(mainClassEdit);
        layout->addWidget(convertBtn);
        layout->addWidget(logText);
    }

private slots:
    void selectInput() {
        QString filePath = QFileDialog::getOpenFileName(this, "Select Chisel Source File", "", "Scala Files (*.scala);;All Files (*)");
        if (!filePath.isEmpty()) {
            inputPath = filePath;
            inputLabel->setText("Input Scala File: " + filePath);
        }
    }

    void selectOutput() {
        QString dirPath = QFileDialog::getExistingDirectory(this, "Select Output Directory");
        if (!dirPath.isEmpty()) {
            outputPath = dirPath;
            outputLabel->setText("Output Directory: " + dirPath);
        }
    }

    void convert() {
        if (inputPath.isEmpty() || outputPath.isEmpty()) {
            QMessageBox::warning(this, "Error", "Please select both an input file and an output directory.");
            return;
        }

        logText->append("<hr>Starting conversion...");

        // Check if sbt (Scala Build Tool) is available
        QProcess checkSbt;
        checkSbt.start("sbt", QStringList() << "--version");
        if (!checkSbt.waitForStarted(3000)) {
            QMessageBox::critical(this, "Error", "sbt is not installed or not found in PATH.\nChisel code must be compiled via sbt.");
            return;
        }
        checkSbt.waitForFinished();

        // Check if firtool (CIRCT) is available (required by Chisel 5+)
        QProcess checkFirtool;
        checkFirtool.start("firtool", QStringList() << "--version");
        if (!checkFirtool.waitForStarted(3000)) {
            QMessageBox::critical(this, "Error", "firtool is not installed or not found in PATH.\nChisel 5 uses CIRCT to generate Verilog and requires the firtool binary.");
            return;
        }
        checkFirtool.waitForFinished();

        convertBtn->setEnabled(false);

        if (tempDir) delete tempDir;
        tempDir = new QTemporaryDir();
        if (!tempDir->isValid()) {
            QMessageBox::critical(this, "Error", "Failed to create a temporary build directory.");
            convertBtn->setEnabled(true);
            return;
        }
        tempDir->setAutoRemove(false); // We manage cleanup manually based on success/failure

        QString tempPath = tempDir->path();
        logText->append("Using temporary build directory: " + tempPath);

        // Copy input file to temp dir
        QFileInfo inputFileInfo(inputPath);
        QString tempInputFile = tempPath + "/" + inputFileInfo.fileName();
        if (!QFile::copy(inputPath, tempInputFile)) {
            QMessageBox::critical(this, "Error", "Failed to copy input file to temporary directory.");
            convertBtn->setEnabled(true);
            return;
        }

        // Create dynamic build.sbt for Chisel 5.0.0
        QString buildSbtPath = tempPath + "/build.sbt";
        QFile buildSbt(buildSbtPath);
        if (buildSbt.open(QIODevice::WriteOnly | QIODevice::Text)) {
            QTextStream out(&buildSbt);
            out << "scalaVersion := \"2.13.12\"\n\n";
            out << "resolvers += Resolver.sonatypeRepo(\"releases\")\n\n";
            out << "addCompilerPlugin(\"org.chipsalliance\" % \"chisel-plugin\" % \"5.0.0\" cross CrossVersion.full)\n";
            out << "libraryDependencies += \"org.chipsalliance\" %% \"chisel\" % \"5.0.0\"\n";
            buildSbt.close();
        } else {
            QMessageBox::critical(this, "Error", "Failed to create build.sbt.");
            convertBtn->setEnabled(true);
            return;
        }

        // Specify modern SBT version
        QDir projDir(tempPath + "/project");
        projDir.mkpath(".");
        QFile buildProps(projDir.path() + "/build.properties");
        if (buildProps.open(QIODevice::WriteOnly | QIODevice::Text)) {
            QTextStream out(&buildProps);
            out << "sbt.version=1.9.7\n";
            buildProps.close();
        }

        // Execute SBT Process
        QProcess *process = new QProcess(this);
        process->setWorkingDirectory(tempPath);

        connect(process, &QProcess::readyReadStandardOutput, this, [=]() {
            QString out = QString::fromUtf8(process->readAllStandardOutput()).trimmed();
            if (!out.isEmpty()) logText->append(out);
        });
        connect(process, &QProcess::readyReadStandardError, this, [=]() {
            QString err = QString::fromUtf8(process->readAllStandardError()).trimmed();
            if (!err.isEmpty()) logText->append("<font color='orange'>" + err + "</font>");
        });
        connect(process, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), this, [=, this](int exitCode, QProcess::ExitStatus exitStatus) {
            if (exitCode == 0 && exitStatus == QProcess::NormalExit) {
                logText->append("<font color='green'>Compilation successful! Extracting Verilog files...</font>");
                moveOutputFiles(tempPath);
                if (tempDir) {
                    tempDir->remove(); // Clean up on success
                    delete tempDir;
                    tempDir = nullptr;
                }
            } else {
                logText->append("<font color='red'>Compilation failed. Check the logs above. Temp dir kept at: " + tempPath + "</font>");
                if (tempDir) {
                    delete tempDir; // Leave folder for debugging but free memory
                    tempDir = nullptr;
                }
            }
            process->deleteLater();
            convertBtn->setEnabled(true);
        });

        QString mainClass = mainClassEdit->text().trimmed();
        if (mainClass.isEmpty()) mainClass = "Main";

        logText->append("Running sbt with main class: " + mainClass);
        // Disable supershell to prevent terminal escape codes in the log view
        process->start("sbt", QStringList() << "-Dsbt.supershell=false" << ("runMain " + mainClass));
    }

private:
    void moveOutputFiles(const QString &tempPath) {
        int movedCount = 0;
        // Search subdirectories recursively as SBT or firtool may place files in target/
        QDirIterator it(tempPath, QStringList() << "*.v" << "*.sv" << "*.fir", QDir::Files, QDirIterator::Subdirectories);

        while (it.hasNext()) {
            QString src = it.next();
            QString fileName = it.fileName();
            QString dest = outputPath + "/" + fileName;

            if (QFile::exists(dest)) QFile::remove(dest);

            if (QFile::copy(src, dest)) {
                movedCount++;
                logText->append("<font color='green'>Copied: " + fileName + " to " + outputPath + "</font>");
            } else {
                logText->append("<font color='red'>Failed to copy: " + fileName + "</font>");
            }
        }

        if (movedCount > 0) {
            QMessageBox::information(this, "Success", QString("Converted %1 files successfully!").arg(movedCount));
        } else {
            logText->append("<font color='red'>No Verilog (.v/.sv) files were found in the generated build tree.</font>");
            QMessageBox::warning(this, "Warning", "No Verilog (.v/.sv) files were generated or found.");
        }
    }

    QLabel *inputLabel;
    QLabel *outputLabel;
    QTextEdit *logText;
    QLineEdit *mainClassEdit;
    QPushButton *convertBtn;

    QString inputPath;
    QString outputPath;
    QTemporaryDir *tempDir = nullptr;
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    ChiselConverter window;
    window.show();
    return app.exec();
}

#include "main.moc"

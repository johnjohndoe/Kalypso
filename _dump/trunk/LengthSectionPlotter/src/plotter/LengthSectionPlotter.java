/**
 * 
 */
package plotter;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

import javax.swing.JFrame;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.tuhh.core.wspwin.strang.LngSink;

/**
 * @author kimwerner
 * 
 */
public class LengthSectionPlotter extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7262571554369474564L;

	/**
	 * 
	 */
	public LengthSectionPlotter() {
	}

	// private final File findPlotter(final Shell shell) {
	//
	// String plotterPath = "C:\\Programme\\bce\\WspWin Plotter\\plotter.exe";
	//
	// File file = new File(plotterPath);
	// if (file.exists())
	// return file;
	//
	// final FileDialog dlg = new FileDialog(shell);
	// plotterPath = dlg.open();
	// if (plotterPath == null)
	// return null;
	//
	// return new File(plotterPath);
	// }

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		Display display = new Display();
		final Shell shell = new Shell(display);
		shell.setLayout(new GridLayout(1, false));
		final Composite cmp = new Composite(shell, SWT.None);
		cmp.setLayout(new GridLayout(2, false));
		cmp.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final Text source = new Text(cmp, SWT.TRAIL | SWT.SINGLE | SWT.BORDER);
		source.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		source.setText(args[1]);
		final Button sourceFD = new Button(cmp, SWT.NONE);
		sourceFD
				.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		sourceFD.setText("CSV");
		sourceFD.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				source.setText(new FileDialog(shell).open());

			}
		});
		final Text target = new Text(cmp, SWT.TRAIL | SWT.SINGLE | SWT.BORDER);
		target.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		target.setText(args[0]);
		final Button targetFD = new Button(cmp, SWT.NONE);
		targetFD.setText("LNG,EXE");
		targetFD
				.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		targetFD.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				target.setText(new FileDialog(shell).open());

			}
		});

		final Label success = new Label(shell, SWT.NONE);
		success.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		final Button ok = new Button(shell, SWT.NONE);
		ok.setText("starten");
		ok.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		ok.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {

				try {
					if (target.getText().endsWith(".exe")) {
						final File file = new File(System
								.getProperty("java.io.tmpdir"), "exportTmp.lng");//$NON-NLS-1$ //$NON-NLS-2$
						final LngSink sink = new LngSink(new FileReader(source
								.getText()), new PrintWriter(
								new FileOutputStream(file)));
						sink.read(';', 0);
						sink.write();
						Runtime
								.getRuntime()
								.exec(
										"\""	+ target.getText() + "\" \"" + file.getPath() + "\"");//$NON-NLS-1$ //$NON-NLS-2$// $NON-NLS-3$
					} else {
						final LngSink sink = new LngSink(source.getText(),
								target.getText());
						sink.read();
						sink.write();
						success.setText("well done");
					}

				} catch (IOException e1) {
					// TODO Auto-generated catch block
					success
							.setText(e1.getLocalizedMessage().equals("") ? "Datei nicht gefunden"
									: e1.getLocalizedMessage());
				}
				shell.layout();
			}
		});

		shell.layout();
		shell.setText("Längschnitt Generator");

		shell.open();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				// If no more entries in event queue
				display.sleep();
			}
		}
	}
}

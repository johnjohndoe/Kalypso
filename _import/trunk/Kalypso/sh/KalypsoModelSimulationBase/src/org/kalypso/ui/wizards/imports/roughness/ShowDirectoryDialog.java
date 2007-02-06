/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.wizards.imports.roughness;

//Send questions, comments, bug reports, etc. to the authors:

//Rob Warner (rwarner@interspatial.com)
//Robert Harris (rbrt_harris@yahoo.com)

import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
* This class demonstrates the DirectoryDialog class
*/
public class ShowDirectoryDialog {
/**
 * Runs the application
 */
public void run() {
  Display display = new Display();
  Shell shell = new Shell(display);
  shell.setText("Directory Browser");
  createContents(shell);
  shell.pack();
  shell.open();
  while (!shell.isDisposed()) {
    if (!display.readAndDispatch()) {
      display.sleep();
    }
  }
}

/**
 * Creates the window contents
 * 
 * @param shell the parent shell
 */
private void createContents(final Shell shell) {
  shell.setLayout(new GridLayout(6, true));
  new Label(shell, SWT.NONE).setText("Directory:");

  // Create the text box extra wide to show long paths
  final Text text = new Text(shell, SWT.BORDER);
  GridData data = new GridData(GridData.FILL_HORIZONTAL);
  data.horizontalSpan = 4;
  text.setLayoutData(data);

  // Clicking the button will allow the user
  // to select a directory
  Button button = new Button(shell, SWT.PUSH);
  button.setText("Browse...");
  button.addSelectionListener(new SelectionAdapter() {
    public void widgetSelected(SelectionEvent event) {
      DirectoryDialog dlg = new DirectoryDialog(shell);

      // Set the initial filter path according
      // to anything they've selected or typed in
      dlg.setFilterPath(text.getText());

      // Change the title bar text
      dlg.setText("SWT's DirectoryDialog");

      // Customizable message displayed in the dialog
      dlg.setMessage("Select a directory");

      // Calling open() will open and run the dialog.
      // It will return the selected directory, or
      // null if user cancels
      String dir = dlg.open();
      if (dir != null) {
        // Set the text box to the new selection
        text.setText(dir);
      }
    }
  });
}

/**
 * The application entry point
 * 
 * @param args the command line arguments
 */
public static void main(String[] args) {
  new ShowDirectoryDialog().run();
}
}
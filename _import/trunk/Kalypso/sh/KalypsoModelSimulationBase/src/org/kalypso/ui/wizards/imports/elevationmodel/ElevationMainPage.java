package org.kalypso.ui.wizards.imports.elevationmodel;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.wizards.imports.Messages;


/**
 * @author Madanagopal
 */
public class ElevationMainPage extends WizardPage {
  private Text sourceFileField;
  private IPath initialSourcePath;
  List<String> fileExtensions = new LinkedList<String>();

  public ElevationMainPage() {
     super(Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ));
     setTitle(Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ));
    // setDescription(Messages.getString( "ElevationWizard.2" ));
  }

  /**
   * Creates the top level control for this dialog page under the
   * given parent composite, then calls <code>setControl</code> so
   * that the created control can be accessed via
   * <code>getControl</code>
   * 
   * @param parent the parent composite
   */
  public void createControl(Composite parent) {
     Composite container = new Composite(parent, SWT.NULL);
     final GridLayout gridLayout = new GridLayout();
     gridLayout.numColumns = 3;
     container.setLayout(gridLayout);
     setControl(container);

     final Label label = new Label(container, SWT.NONE);
     final GridData gridData = new GridData();
     gridData.horizontalSpan = 3;
     label.setLayoutData(gridData);
     label.setText(Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.1" ));

     final Label label_1 = new Label(container, SWT.NONE);
     final GridData gridData_1 =
        new GridData(GridData.HORIZONTAL_ALIGN_END);
     label_1.setLayoutData(gridData_1);
     label_1.setText(Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.2" ));

     sourceFileField = new Text(container, SWT.BORDER);
     sourceFileField.addModifyListener(new ModifyListener() {
        public void modifyText(ModifyEvent e) {
           updatePageComplete();
        }
     });
     sourceFileField.setLayoutData(
        new GridData(GridData.FILL_HORIZONTAL));

     final Button button = new Button(container, SWT.NONE);
     button.addSelectionListener(new SelectionAdapter() {
        public void widgetSelected(SelectionEvent e) {
           browseForSourceFile();
        }
     });
     button.setText("Browse..");

     initContents();
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * @param selection the selection or <code>null</code> if none
   */
  public void init(ISelection selection) {
     if (!(selection instanceof IStructuredSelection))
        return;
     
     fileExtensions.add( new String ("asc") );
         
     // Find the first plugin.xml file.
     Iterator iter = ((IStructuredSelection) selection).iterator();
     while (iter.hasNext()) {
        Object item = (Object) iter.next();
        if (item instanceof IFile) {
           IFile file = (IFile) item;
           if (fileExtensions.contains(file.getFileExtension())){
              initialSourcePath = file.getLocation();
              break;
           }
           item = file.getProject();
        }
        }
  }

  /**
   * Called by <code>createControl</code> to initialize the receiver's
   * content based upon the cached selection provided by the wizard.
   */
  private void initContents() {
     if (initialSourcePath == null)
        return;
     IPath rootLoc = ResourcesPlugin.getWorkspace()
        .getRoot().getLocation();
     IPath path = initialSourcePath;
     if (rootLoc.isPrefixOf(path))
        path = path
           .setDevice(null)
           .removeFirstSegments(rootLoc.segmentCount());
     sourceFileField.setText(path.toString());
     updatePageComplete();
     setMessage(null);
     setErrorMessage(null);
  }

  /**
   * Update the current page complete state
   * based on the field content.
   */
  private void updatePageComplete() {
     setPageComplete(false);

     IPath sourceLoc = getSourceLocation();
     if (sourceLoc == null || !(fileExtensions.contains( sourceLoc.getFileExtension() ))) {
        setMessage(null);
        setErrorMessage(Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.3" ));
        return;
     }
     setPageComplete(true);
     setMessage(null);
     setErrorMessage(null);
  }

  /**
   * Open a file browser dialog to locate a source file
   */
  protected void browseForSourceFile() {
     IPath path = browse(getSourceLocation(), false);
     if (path == null)
        return;
     IPath rootLoc = ResourcesPlugin.getWorkspace()
        .getRoot().getLocation();
     if (rootLoc.isPrefixOf(path))
        path = path
           .setDevice(null)
           .removeFirstSegments(rootLoc.segmentCount());
     sourceFileField.setText(path.toString());
  }
  

  /**
   * Open a file dialog for selecting a file
   * 
   * @param path the initially selected file
   * @param mustExist <code>true</code> if the selected
   *           file must already exist, else <code>false</code>
   * @return the newly selected file or <code>null</code>
   */
  private IPath browse(IPath path, boolean mustExist) {
     FileDialog dialog = new FileDialog(getShell(),SWT.OPEN);
     if (path != null) {
        if (path.segmentCount() > 1)
           dialog.setFilterPath(path.removeLastSegments(1)
                 .toOSString());
        if (path.segmentCount() > 0)
           dialog.setFileName(path.lastSegment());
     }
     String result = dialog.open();
     if (result == null)
        return null;
     return new Path(result);
  }

  /**
   * Answer the source file location
   * or <code>null</code> if unspecified
   */
  public IPath getSourceLocation() {
     String text = sourceFileField.getText().trim();
     if (text.length() == 0)
        return null;
     IPath path = new Path(text);
     if (!path.isAbsolute())
        path = ResourcesPlugin.getWorkspace().getRoot().getLocation()
              .append(path);
     return path;
  }
}


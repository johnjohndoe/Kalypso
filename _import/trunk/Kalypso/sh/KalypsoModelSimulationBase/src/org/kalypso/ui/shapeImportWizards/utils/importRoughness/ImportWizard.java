/**
 * 
 */
package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 *
 */
public class ImportWizard extends Wizard implements INewWizard {
  protected PageMain pageMain;		// wizard page
//  protected PageStyleSelection pageStyleSelection;
  protected DataContainer m_data;	// the data model
//  private GisMapOutlineViewer m_outlineviewer;
//  private GmlFileImportPage pageGmlFileImport;
  protected IStructuredSelection selection;	// workbench selection when the wizard was started
  protected boolean wizardCompleted = false;	// flag indicated whether the wizard can be completed or not
  protected IWorkbench workbench;	// the workbench instance - NOT USED HERE

  public ImportWizard() {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor(true);
  }

  /**
   * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
   */
  public void init(IWorkbench workbench, IStructuredSelection selection) 
  {
    this.workbench = workbench;
    this.selection = selection;
    setWindowTitle( "Shape import" );
  }

  @Override
  public void addPages()
  {
    pageMain = new PageMain(selection);
//    pageStyleSelection = new PageStyleSelection();
    addPage(pageMain);
//    addPage(pageStyleSelection);
//    pageGmlFileImport = new GmlFileImportPage( "GML:importPage", "Hinzufügen einer GML-Datei (im Workspace) zu einer Karte", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    //pageGmlFileImport.setProjectSelection( m_outlineviewer.getMapModell().getProject() );

//    addPage( pageGmlFileImport );
  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
//  public void setOutlineViewer( GisMapOutlineViewer outlineviewer )
//  {
//    m_outlineviewer = outlineviewer;
//  }

  @Override
  public boolean canFinish()
  {
    wizardCompleted = pageMain.cmb_ShapeProperty.isEnabled() && pageMain.isTextNonEmpty(pageMain.txt_OutputFile);
    return wizardCompleted;
  }

  @Override
  public boolean performFinish() 
  {
    pageMain.saveDataToModel();
//    Feature[] features = pageGmlFileImport.getFeatures();
    final ICoreRunnableWithProgress operation = new TransformerShapeToIRoughnessCollection( m_data); //$NON-NLS-1$
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation);
    ErrorDialog.openError(getShell(), getWindowTitle(), "", status );
    return status.isOK();
  }
}

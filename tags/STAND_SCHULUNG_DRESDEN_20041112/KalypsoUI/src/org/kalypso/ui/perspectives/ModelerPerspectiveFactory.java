package org.kalypso.ui.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * The perspective for the Kalypso Modeler.
 * 
 * @author schlienger
 */
public class ModelerPerspectiveFactory implements IPerspectiveFactory
{
  /**
   * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
   */
  public void createInitialLayout( final IPageLayout layout )
  {
    // Editors are placed for free.
    final String editorArea = layout.getEditorArea();

    // Top left.
    final IFolderLayout topLeft = layout.createFolder("topLeft", IPageLayout.LEFT, (float)0.26, editorArea);//$NON-NLS-1$
    topLeft.addView(IPageLayout.ID_RES_NAV);

    // Bottom left.
    IFolderLayout bottomLeft = layout.createFolder("bottomLeft", IPageLayout.BOTTOM, (float)0.50,//$NON-NLS-1$
      "topLeft");//$NON-NLS-1$
    bottomLeft.addView(IPageLayout.ID_OUTLINE);

    setContentsOfShowViewMenu( layout );
    
    layout.addActionSet( "org.kalypso.actionSet.model" );
    
    layout.addNewWizardShortcut( "org.kalypso.wizard.newMap" );
    layout.addNewWizardShortcut( "org.kalypso.wizard.newGtt" );
    layout.addNewWizardShortcut( "org.kalypso.wizard.newOdt" );
    layout.addNewWizardShortcut( "org.kalypso.wizard.newOtt" );
    layout.addNewWizardShortcut( "org.kalypso.wizard.newCalcCase" );
    
    layout.setFixed( true );
  }
  
  /**
   * Sets the intial contents of the "Show View" menu.
   */
  protected void setContentsOfShowViewMenu( IPageLayout layout )
  {
    layout.addShowViewShortcut( IPageLayout.ID_OUTLINE );
    layout.addShowViewShortcut( IPageLayout.ID_RES_NAV );
    
    layout.addShowViewShortcut( "org.kalypso.ui.repository.view.RepositoryExplorerPart" );
    layout.addShowViewShortcut( "org.kalypso.ogc.sensor.view.DiagramViewPart" );
    layout.addShowViewShortcut( "org.kalypso.ogc.sensor.view.TableViewPart" );
  }
  
  
}
package org.kalypso.ui.editor.diagrameditor.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.diagrameditor.ObsDiagOutlinePage;

/**
 * RemoveThemeAction
 * 
 * @author schlienger
 */
public class RemoveThemeAction extends FullAction implements ISelectionChangedListener
{
  private ObsDiagOutlinePage m_page;

  public RemoveThemeAction( ObsDiagOutlinePage page )
  {
    super( "Thema entfernen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE, "Entfernt aktives Thema" );

    m_page = page;
    
    m_page.addSelectionChangedListener( this );
  }
  
  public void dispose()
  {
    m_page.removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    final IDiagramTemplateTheme selectedTheme = m_page.getSelectedTheme();
    
    if( selectedTheme != null && MessageDialog.openConfirm( m_page.getSite().getShell(),  "Zeitreihe entfernen", "Wollen Sie wirklich die Zeitreihe " + selectedTheme.getName()+" entfernen" ) )
        m_page.getTemplate().removeTheme( selectedTheme );
  }
  
  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( m_page.isThemeSelected() );
  }
}

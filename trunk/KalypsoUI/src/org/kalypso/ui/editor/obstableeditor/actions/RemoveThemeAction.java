package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.obstableeditor.ObsTableOutlinePage;

/**
 * RemoveThemeAction
 * 
 * @author schlienger
 */
public class RemoveThemeAction extends FullAction
{
  private ObsTableOutlinePage m_page;

  public RemoveThemeAction( ObsTableOutlinePage page )
  {
    super( "Spalte entfernen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE, "Entfernt aktive Spalte" );

    m_page = page;
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    final ITableViewTheme selectedTheme = m_page.getSelectedTheme();
    
    if( selectedTheme != null && MessageDialog.openConfirm( m_page.getSite().getShell(), "Zeitreihe entfernen", "Wollen Sie wirklich die Zeitreihe " + selectedTheme.getName() ) )
        m_page.getTemplate().removeTheme( selectedTheme );
  }
}

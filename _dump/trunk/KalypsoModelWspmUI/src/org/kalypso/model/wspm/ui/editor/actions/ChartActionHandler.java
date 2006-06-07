/**
 * 
 */
package org.kalypso.model.wspm.ui.editor.actions;

import org.eclipse.ui.IEditorPart;
import org.kalypso.contribs.eclipse.ui.AbstractEditorPartAction;
import org.kalypso.contribs.eclipse.ui.IEditorPartAction;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartActionsEnum;


/**
 * @author Belger
 */
public class ChartActionHandler extends AbstractEditorPartAction implements IEditorPartAction
{
  private final ProfilChartActionsEnum m_chartAction;

  public ChartActionHandler( final ProfilChartActionsEnum chartAction )
  {
    super( "", chartAction.getStyle() );
    
    m_chartAction = chartAction;
  }

  @Override
  public void run( )
  {
    final IEditorPart editorPart = getEditorPart();
    if( editorPart instanceof ProfilchartEditor)
    {
      final ProfilchartEditor editor = (ProfilchartEditor)editorPart;
      editor.runChartAction( m_chartAction );
    }
  }
  
}

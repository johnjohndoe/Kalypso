package org.kalypso.ui.editor.diagrameditor.actions;

import java.util.Vector;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.impl.ObservationDiagramTemplate;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.diagrameditor.ObsDiagOutlinePage;

/**
 * SwitchAxisTypePulldownActionDelegate
 * 
 * @author schlienger
 */
public class FilterAxesAction extends FullAction
{
  private static final String TITLE = "Achsentyp ignorieren";

  private final static String MSG = "Wählen Sie der Achsentyp welche nicht dargestellt werden soll";

  private ObsDiagOutlinePage m_page;

  public FilterAxesAction( ObsDiagOutlinePage page )
  {
    super( "Achsen filtern", ImageProvider.IMAGE_UTIL_FILTER,
        "Erlaubt die Deaktivierung einer Achsentyp" );

    m_page = page;
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    final ObservationDiagramTemplate tpl = m_page.getTemplate();

    final Vector elts = new Vector( tpl.getDiagramAxes() );
    elts.add( 0, "(Keiner)" );

    final ListDialog dlg = new ListDialog( m_page.getSite().getShell() );

    dlg.setInput( elts.toArray() );
    dlg.setContentProvider( new ArrayContentProvider() );
    dlg.setLabelProvider( new LabelProvider() );
    dlg.setMessage( MSG );
    dlg.setTitle( TITLE );

    if( dlg.open() == Window.OK )
    {
      final Object[] res = dlg.getResult();

      if( res.length == 1 && res[0] instanceof IDiagramAxis )
        tpl.setIgnoreType( ((IDiagramAxis) res[0]).getLabel() );
      else
        tpl.setIgnoreType( null );
      
      tpl.fireTemplateChanged( new TemplateEvent( tpl.getThemes(), TemplateEvent.TYPE_REFRESH ) );
    }
  }
}
package org.kalypso.ogc.gml.table.celleditors;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.sensor.jface.ObservationLinkDialog;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author Belger
 */
public class ObservationFeatureCellEditor extends AbstractFeatureCellEditor
{
  public ObservationFeatureCellEditor( )
  {
    // TODO: set context instead of null
    setCellEditor( new ObservationDialogEditor( null ) );
    setValidator( DefaultCellValidators.DOUBLE_VALIDATOR );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    final Object value = getEditor().getValue();

    final Map map = new HashMap();
    map.put( getPropertyName(), value );

    return map;
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doSetFeature(org.deegree.model.feature.Feature)
   */
  protected void doSetFeature( final Feature feature )
  {
    final Object value = feature.getProperty( getPropertyName() );
    getEditor().setValue( value );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#renderLabel(org.deegree.model.feature.Feature)
   */
  public String renderLabel( Feature feature )
  {
    final TimeseriesLink link = (TimeseriesLink)feature.getProperty( getPropertyName() );
    return link == null ? "<kein Wert>" : "Zeitreihe: " + link.getHref();
  }

  private final static class ObservationDialogEditor extends DialogCellEditor
  {
    private final URL m_context;

    public ObservationDialogEditor( final URL context )
    {
      m_context = context;
    }

    /**
     * @see org.kalypso.ogc.gml.table.celleditors.DialogCellEditor#openDialog(org.eclipse.swt.widgets.Control)
     */
    protected boolean openDialog( final Control control )
    {
      final TimeseriesLink obslink = (TimeseriesLink)doGetValue();

      final ObservationLinkDialog dialog = new ObservationLinkDialog( control.getShell(), obslink,
          m_context );

      boolean b = false;

      if( dialog.open() == Window.OK )
      {
        doSetValue( dialog.getResult() );

        b = true;
      }

      dialog.dispose();

      return b;
    }
  }

}
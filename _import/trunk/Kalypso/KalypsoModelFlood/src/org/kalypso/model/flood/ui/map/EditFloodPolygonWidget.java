package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.ui.map.AbstractEditFeatureWidget;
import org.kalypso.model.flood.binding.IFloodPolygon;

/**
 * @author Gernot Belger
 */
public class EditFloodPolygonWidget extends AbstractEditFeatureWidget
{
  public EditFloodPolygonWidget( )
  {
    super( "WSP-Anpassungen l�schen", "l�scht WSP-Anpassungen", false, new QName[] { IFloodPolygon.QNAME }, IFloodPolygon.QNAME_PROP_AREA );
  }
}

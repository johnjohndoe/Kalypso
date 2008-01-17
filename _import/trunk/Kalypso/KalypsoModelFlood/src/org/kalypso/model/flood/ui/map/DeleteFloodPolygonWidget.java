package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.ui.map.AbstractDeleteFeatureWidget;
import org.kalypso.model.flood.binding.IFloodPolygon;

/**
 * @author Gernot Belger
 */
public class DeleteFloodPolygonWidget extends AbstractDeleteFeatureWidget
{
  public DeleteFloodPolygonWidget( )
  {
    super( "WSP-Anpassungen löschen", "löscht WSP-Anpassungen", true, new QName[] { IFloodPolygon.QNAME }, IFloodPolygon.QNAME_PROP_AREA );
  }
}

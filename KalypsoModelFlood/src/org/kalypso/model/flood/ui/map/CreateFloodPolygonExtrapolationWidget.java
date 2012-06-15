package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodExtrapolationPolygon;
import org.kalypso.ogc.gml.map.widgets.newfeature.AbstractCreateGeometryWidget;

/**
 * @author Gernot Belger
 */
public class CreateFloodPolygonExtrapolationWidget extends AbstractCreateGeometryWidget
{
  public CreateFloodPolygonExtrapolationWidget( )
  {
    super( IFloodExtrapolationPolygon.QNAME, new QName[] { IFloodExtrapolationPolygon.QNAME_PROP_AREA, IFloodExtrapolationPolygon.QNAME_PROP_REFPOINT } );
  }
}

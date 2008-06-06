package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodVolumePolygon;
import org.kalypso.ogc.gml.map.widgets.newfeature.AbstractCreateGeometryWidget;

/**
 * @author Gernot Belger
 */
public class CreateFloodPolygonVolumeWidget extends AbstractCreateGeometryWidget
{
  public CreateFloodPolygonVolumeWidget( )
  {
    super( IFloodVolumePolygon.QNAME, new QName[] { IFloodVolumePolygon.QNAME_PROP_AREA } );
  }
}

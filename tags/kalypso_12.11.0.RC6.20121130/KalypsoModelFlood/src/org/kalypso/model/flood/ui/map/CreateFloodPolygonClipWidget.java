package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodClipPolygon;
import org.kalypso.ogc.gml.map.widgets.newfeature.AbstractCreateGeometryWidget;

/**
 * @author Gernot Belger
 */
public class CreateFloodPolygonClipWidget extends AbstractCreateGeometryWidget
{
  public CreateFloodPolygonClipWidget( )
  {
    super( IFloodClipPolygon.QNAME, new QName[] { IFloodClipPolygon.QNAME_PROP_AREA } );
  }
}

package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodClipPolygon;

/**
 * @author Gernot Belger
 */
public class CreateFloodPolygonClipWidget extends AbstractCreateFloodPolygonWidget
{
  public CreateFloodPolygonClipWidget( )
  {
    super( IFloodClipPolygon.QNAME, new QName[] { IFloodClipPolygon.QNAME_PROP_AREA } );
  }
}

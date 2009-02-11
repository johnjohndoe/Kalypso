package org.kalypso.model.flood.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodVolumePolygon;

/**
 * @author Gernot Belger
 */
public class CreateFloodPolygonVolumeWidget extends AbstractCreateFloodPolygonWidget
{
  public CreateFloodPolygonVolumeWidget( )
  {
    super( IFloodVolumePolygon.QNAME, new QName[] { IFloodVolumePolygon.QNAME_PROP_AREA } );
  }
}

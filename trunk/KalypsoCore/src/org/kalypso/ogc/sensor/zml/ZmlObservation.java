package org.kalypso.ogc.sensor.zml;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * ZmlObservation. Nothing else than a SimpleObservation but with the href information.
 * 
 * @author schlienger
 */
public class ZmlObservation extends SimpleObservation
{
  private final String m_href;

  /**
   * Constructor.
   * 
   * @param href
   * @param identifier
   * @param name
   * @param editable
   * @param target
   * @param metadata
   * @param axes
   */
  public ZmlObservation( String href, String identifier, String name, boolean editable,
      IXlink target, MetadataList metadata, IAxis[] axes )
  {
    super( identifier, name, editable, target, metadata, axes );

    m_href = href;
  }
  
  /**
   * @param href
   * @param identifier
   * @param name
   * @param editable
   * @param target
   * @param metadata
   * @param axes
   * @param model
   */
  public ZmlObservation( String href, String identifier, String name, boolean editable,
      IXlink target, MetadataList metadata, IAxis[] axes, ITuppleModel model )
  {
    super( identifier, name, editable, target, metadata, axes, model );

    m_href = href;
  }
  
  /**
   * @return Returns the href.
   */
  public String getHref( )
  {
    return m_href;
  }
}

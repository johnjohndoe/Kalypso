package org.kalypso.ogc.sensor.template;

import java.util.Properties;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.impl.DiagramCurve;
import org.kalypso.util.link.ObjectLink;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * @author schlienger
 */
public class LinkedDiagramCurve extends ObjectLink implements IDiagramCurve
{
  private final String m_name;

  private final IDiagramTemplate m_template;

  private final Properties m_mappings;

  private DiagramCurve m_curve = null;

  public LinkedDiagramCurve( final String linkType, final IXlink xlink, final String name, final Properties mappings, final IDiagramTemplate template )
  {
    super( linkType, xlink );

    m_name = name;
    m_mappings = mappings;
    m_template = template;
  }

  public boolean equals( Object obj )
  {
    return m_curve.equals( obj );
  }

  public IAxisMapping[] getMappings()
  {
    return m_curve.getMappings();
  }

  public String getName()
  {
    return m_curve.getName();
  }

  public IObservation getObservation()
  {
    return m_curve.getObservation();
  }

  /**
   * @see org.kalypso.util.link.ObjectLink#linkResolved(java.lang.Object)
   */
  public void linkResolved( Object object )
  {
    super.linkResolved( object );

    m_curve = new DiagramCurve( m_name, (IObservation)getLinkedObject(), m_mappings, m_template );
  }
}
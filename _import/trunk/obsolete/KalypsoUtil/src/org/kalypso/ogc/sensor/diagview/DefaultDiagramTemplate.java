package org.kalypso.ogc.sensor.diagview;

import java.util.Hashtable;
import java.util.Map;
import java.util.Vector;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.template.AbstractTemplateAdapter;

/**
 * A default template for an observation.
 * 
 * @author schlienger
 */
public class DefaultDiagramTemplate extends AbstractTemplateAdapter implements IDiagramTemplate
{
  private final IObservation m_obs;
  private final Map m_diagramAxisMap = new Hashtable();
  private ICurve[] m_curves;

  public DefaultDiagramTemplate( final IObservation obs )
  {
    m_obs = obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getTitle()
   */
  public String getTitle()
  {
    return m_obs.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getLegendName()
   */
  public String getLegendName()
  {
    return "Legende";
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#isShowLegend()
   */
  public boolean isShowLegend()
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getAxisList()
   */
  public IDiagramAxis[] getAxisList()
  {
    // be sure to call getCurveList() since it creates the diagram axis as a side effect
    getCurveList();
    
    return (IDiagramAxis[])m_diagramAxisMap.values().toArray( new IDiagramAxis[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getCurveList()
   */
  public ICurve[] getCurveList()
  {
    if( m_curves == null )
    {
      final IAxis[] oAxes = m_obs.getAxisList();
      
      if( oAxes.length < 2 )
        return null;
  
      final Vector curves = new Vector();
  
      // common mapping: first axis of observation
      final IAxis oCmn = oAxes[0];
      final IDiagramAxis dCmn = createDiagramAxis( m_diagramAxisMap, oCmn, IDiagramAxis.DIRECTION_HORIZONTAL, IDiagramAxis.POSITION_BOTTOM, false );
      final IAxisMapping mCmn = new DefaultMapping( oCmn, dCmn );
      
      // for each subsequent axis of observation create a curve
      for( int i = 1; i < oAxes.length; i++ )
      {
        final IDiagramAxis da = createDiagramAxis( m_diagramAxisMap, oAxes[i], IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );
        final IAxisMapping m = new DefaultMapping( oAxes[i], da );
        
        curves.add( new DefaultCurve( oAxes[i].getLabel(), m_obs, new IAxisMapping[] { mCmn, m } ) );
      }
      
      m_curves = (ICurve[])curves.toArray( new ICurve[0] );
    }
    
    return m_curves;
  }
  
  /**
   * Helper
   */
  private static IDiagramAxis createDiagramAxis( final Map axisMap, final IAxis axis, final String direction, final String position, final boolean isInverted )
  {
    IDiagramAxis da = (IDiagramAxis)axisMap.get( axis.getUnit() );
    
    if( da == null )
    {
      da = new DefaultDiagramAxis( axis.getLabel(), axis.getUnit(), direction, position, isInverted );
      
      axisMap.put( axis.getUnit(), da );
    }
    
    return da;
  }
}

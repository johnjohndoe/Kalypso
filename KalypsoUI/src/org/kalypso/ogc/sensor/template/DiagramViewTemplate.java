package org.kalypso.ogc.sensor.template;

import java.io.IOException;
import java.io.InputStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Vector;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.sensor.diagview.DefaultDiagramAxis;
import org.kalypso.ogc.sensor.diagview.ICurve;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * Template for diagram
 * 
 * @author schlienger
 */
public class DiagramViewTemplate extends AbstractTemplateAdapter implements IDiagramTemplate
{
  private final static ObjectFactory m_factory = new ObjectFactory();

  private final IFile m_file;

  private ObsdiagviewType m_baseTemplate = null;

  private Map m_axesMap = null;

  private final List m_curves = new Vector();

  private final IProgressMonitor m_monitor;

  private List m_baseCurves = null;

  public DiagramViewTemplate( final IFile file, final IProgressMonitor monitor )
  {
    m_file = file;
    m_monitor = monitor;
    
    loadCurves();
  }

  /**
   * helper
   */
  private ObsdiagviewType getBaseTemplate()
  {
    if( m_baseTemplate == null )
    {
      try
      {
        final InputStream ins = m_file.getContents();
        m_baseTemplate = (ObsdiagviewType)m_factory.createUnmarshaller().unmarshal( ins );
        ins.close();
      }
      catch( JAXBException e )
      {
        // TODO handling
        throw new RuntimeException( e );
      }
      catch( CoreException e )
      {
        // TODO handling
        throw new RuntimeException( e );
      }
      catch( IOException e )
      {
        // TODO handling
        throw new RuntimeException( e );
      }
    }

    return m_baseTemplate;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getTitle()
   */
  public String getTitle()
  {
    return getBaseTemplate().getTitle();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getLegendName()
   */
  public String getLegendName()
  {
    final ObsdiagviewType.LegendType l = getBaseTemplate().getLegend();
    return l == null ? "" : l.getTitle();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#isShowLegend()
   */
  public boolean isShowLegend()
  {
    final ObsdiagviewType.LegendType l = getBaseTemplate().getLegend();
    return l == null ? false : l.isVisible();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getAxisList()
   */
  public IDiagramAxis[] getAxisList()
  {
    if( m_axesMap == null )
    {
      m_axesMap = new Hashtable();

      final List baseAxes = getBaseTemplate().getAxis();

      for( final Iterator it = baseAxes.iterator(); it.hasNext(); )
      {
        final TypeAxis baseAxis = (TypeAxis)it.next();

        m_axesMap.put( baseAxis.getId(), new DefaultDiagramAxis( baseAxis ) );
      }
    }

    return (IDiagramAxis[])m_axesMap.values().toArray( new IDiagramAxis[0] );
  }
  
  private void loadCurves()
  {
    m_baseCurves = getBaseTemplate().getCurve();
    
    m_monitor.beginTask( "Diagrammvorlage laden", m_baseCurves.size() );

    for( final Iterator it = m_baseCurves.iterator(); it.hasNext(); )
    {
      final ObsdiagviewType.CurveType baseCurve = (ObsdiagviewType.CurveType)it.next();

      new Curve( baseCurve, m_file.getProject(), this );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getCurveList()
   */
  public ICurve[] getCurveList()
  {
    return (ICurve[])m_curves.toArray( new ICurve[0]);
  }

  /**
   * Returns the diagram axis which has the given id.
   * 
   * @throws NoSuchElementException
   *           if no axis found
   */
  public IDiagramAxis getDiagramAxis( final String id ) throws NoSuchElementException
  {
    final IDiagramAxis axis = (IDiagramAxis)m_axesMap.get( id );

    if( axis == null )
      throw new NoSuchElementException();

    return axis;
  }

  /**
   * Called when a curve has been loaded.
   */
  public void curveLoaded( final Curve curve )
  {
    m_curves.add( curve );
    
    m_monitor.worked(1);
   
    if( m_curves.size() == m_baseCurves.size() )
      fireTemplateLoaded();
  }
}
package org.kalypso.ogc.sensor.diagview.impl;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * DefaultDiagramTemplateTheme
 * 
 * @author schlienger
 */
public class DefaultDiagramTemplateTheme implements IDiagramTemplateTheme
{
  private final List m_curves = new ArrayList();
  private IObservation m_obs = null;
  private IVariableArguments m_args = null;
  private String m_themeName;

  public DefaultDiagramTemplateTheme()
  {
    this( null );
  }
  
  public DefaultDiagramTemplateTheme( final String themeName )
  {
    m_themeName = themeName;
  }
  
  /**
   * @param obs The obs to set.
   */
  public void setObservation( final IObservation obs )
  {
    m_obs = obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme#getObservation()
   */
  public IObservation getObservation( )
  {
    return m_obs;
  }

  /**
   * @param args
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme#getArguments()
   */
  public IVariableArguments getArguments()
  {
    return m_args;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme#getCurves()
   */
  public List getCurves( )
  {
    return m_curves;
  }
  
  public void addCurve( final IDiagramCurve curve )
  {
    m_curves.add( curve );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme#dispose()
   */
  public void dispose( )
  {
    m_curves.clear();
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    if( m_themeName != null )
      return m_themeName;
    
    if( m_obs != null )
      return m_obs.getName();
    
    return super.toString();
  }
}

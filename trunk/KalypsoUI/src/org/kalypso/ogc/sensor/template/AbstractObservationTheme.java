/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.template;

import java.util.Iterator;
import java.util.Vector;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A Theme for an IObservation
 * 
 * @author schlienger
 */
public abstract class AbstractObservationTheme implements IThemeEventProvider,
    IPoolListener, IObservationListener
{
  private String m_name = null;

  private IObservation m_obs = null;

  private IVariableArguments m_args = null;

  private ResourcePool m_pool = null;

  private Vector m_listeners = null;
  
  /**
   * name of the axis type that should be ignored (can be null, in such case no
   * axis is ignored)
   */
  private String m_ignoreType = null;

  /**
   * when true, the default properties of the underlying observation
   * should be used when creating members of this theme.
   */
  private boolean m_useDefault = false;

  private AbstractObservationTheme( final String name )
  {
    m_name = name;
  }

  public AbstractObservationTheme( String name, IVariableArguments args )
  {
    this( name );

    setArguments( args );
  }

  public void dispose( )
  {
    m_listeners.clear();

    if( m_pool != null )
      m_pool.removePoolListener( this );
    
    if( m_obs != null )
      m_obs.removeListener( this );
  }

  /**
   * @return name of theme if defined, or name of observation, or
   *         super.toString().
   */
  public String getName( )
  {
    if( m_name != null )
      return m_name;

    if( m_obs != null )
      return m_obs.getName();

    return super.toString();
  }

  /**
   * @return true when the name of the theme has been explicitely defined
   */
  public final boolean isNameDefined( )
  {
    return m_name != null;
  }
  
  public String getIgnoreType( )
  {
    return m_ignoreType;
  }
  
  public void setIgnoreType( String ignoreType )
  {
    m_ignoreType = ignoreType;
  }
  
  public boolean isUseDefault( )
  {
    return m_useDefault;
  }
  
  public void setUseDefault( final boolean useDefault )
  {
    m_useDefault = useDefault;
  }

  public IObservation getObservation( )
  {
    return m_obs;
  }

  public final void setObservation( final IObservation obs )
  {
    if( m_obs != null )
    {
      m_obs.removeListener( this );
      m_obs = null;
    }

    if( obs != null )
    {
      m_obs = obs;
      m_obs.addListener( this );
    }
  }

  /**
   * @return [optional] variable arguments that can be used when values are
   *         fetched from the observation
   */
  public IVariableArguments getArguments( )
  {
    return m_args;
  }

  /**
   * This method is declared final because it is called from the constructor
   * 
   * @param args
   */
  public final void setArguments( final IVariableArguments args )
  {
    m_args = args;
  }

  public void addListener( final IThemeEventListener listener )
  {
    if( m_listeners == null )
      m_listeners = new Vector();

    m_listeners.add( listener );
  }

  public final void removeListener( final IThemeEventListener listener )
  {
    if( m_listeners != null )
      m_listeners.remove( listener );
  }

  /**
   * Start loading the observation, this object is added as a pool listener
   * 
   * @param key
   */
  public final void loadObservation( final IPoolableObjectType key )
  {
    if( m_pool == null )
      m_pool = KalypsoGisPlugin.getDefault().getPool();

    m_pool.addPoolListener( this, key );
  }

  /**
   * Remove the observation and inform listeners that theme changed
   * 
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public final void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    setObservation( null );

    afterObservationSet();

    fireThemeChanged();
  }

  /**
   * Set the loaded observation and inform listeners that this theme has changed
   * 
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public final void objectLoaded( IPoolableObjectType key, Object newValue,
      IStatus status )
  {
    final IObservation obs = beforeObservationSet( (IObservation) newValue );

    setObservation( obs );

    afterObservationSet();

    fireThemeChanged();
  }

  /**
   * This method is called before the observation is set on this object.
   * Subclass may overwrite this method to refine the observation being loaded.
   * <p>
   * The default implementation is to return the observation as is.
   * 
   * @param observation
   * @return observation
   */
  protected IObservation beforeObservationSet( IObservation observation )
  {
    return observation;
  }

  /**
   * This method is called after the observation is set on this object. Subclass
   * may overwrite this method to adapt their contents depending on the
   * observation.
   * <p>
   * The default implementation does nothing.
   */
  protected void afterObservationSet( )
  {
    // empty
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IThemeEventProvider#fireThemeChanged()
   */
  public void fireThemeChanged( )
  {
    if( m_listeners != null )
    {
      for( final Iterator it = m_listeners.iterator(); it.hasNext(); )
      {
        final IThemeEventListener tel = (IThemeEventListener) it.next();

        tel.onThemeChanged( this );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationListener#observationChanged(org.kalypso.ogc.sensor.IObservation)
   */
  public void observationChanged( IObservation obs )
  {
    fireThemeChanged();
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    final StringBuffer bf = new StringBuffer();

    if( m_name != null )
      bf.append( m_name );

    if( m_obs != null )
      bf.append( "Thema: " ).append( m_obs.getName() ).append( " (" ).append(
          m_obs.getHref() ).append( ')' );

    if( bf.length() == 0 )
      return super.toString();

    return bf.toString();
  }
}
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

import java.awt.Color;
import java.awt.Stroke;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.PoolableObjectWaiter;

/**
 * A kind of view over observations.
 * <p>
 * The view supports enabled features, which might dictate some of the aspects of the appearance. By default, features
 * are enabled.
 * 
 * @author schlienger
 */
public abstract class ObsView implements IObsViewEventProvider
{
  public static final ItemData DEFAULT_ITEM_DATA = new ItemData( true, null, null );

  /** Additional hints for new items */
  public static class ItemData
  {
    public final Color color;

    public final boolean editable;

    public final Stroke stroke;

    public ItemData( final boolean bEditable, final Color c, final Stroke s )
    {
      this.color = c;
      this.editable = bEditable;
      this.stroke = s;
    }
  }

  private final List<ObsViewItem> m_items = new ArrayList<ObsViewItem>();

  private final List<IObsViewEventListener> m_listeners = new ArrayList<IObsViewEventListener>();

  private final Set<String> m_enabledFeatures = new HashSet<String>();

  /**
   * If set, each add of an observation of one of these types is ignored (no items are added for this observation in
   * this view)
   */
  private String[] m_ignoreTypes = new String[] {};

  /** If set, the concerned items are not displayed. Note: this is not to be confused with ignoreTypes */
  private List m_hiddenTypes = new ArrayList();

  /**
   * By default the timezone is the default one. It can be overriden by the value of the template.
   */
  private TimeZone m_timezone = KalypsoGisPlugin.getDefault().getDisplayTimeZone();

  /**
   * Default constructor: enables all the features
   */
  public ObsView( )
  {
    m_enabledFeatures.add( TimeserieConstants.FEATURE_ALARMLEVEL );
    m_enabledFeatures.add( TimeserieConstants.FEATURE_FORECAST );
  }

  public void dispose( )
  {
    removeAllItems();
  }

  /**
   * This method must be called before items are added. If items were already present in this view, it has no impact on
   * them.
   * 
   * @param ignoreTypes
   *            if null a default empty array is used
   */
  public void setIgnoreTypes( final String[] ignoreTypes )
  {
    if( ignoreTypes == null )
      m_ignoreTypes = new String[0];
    else
      m_ignoreTypes = ignoreTypes;
  }

  public String[] getIgnoreTypes( )
  {
    return m_ignoreTypes;
  }

  /**
   * Return a correct string representation
   * 
   * @see java.lang.Object#toString()
   */
  @Override
  public abstract String toString( );

  /**
   * Print the document
   */
  public void print( )
  {
    firePrintObsView( null );
  }

  public void removeAllItems( )
  {
    synchronized( m_items )
    {
      for( final Object element2 : m_items )
      {
        final ObsViewItem element = (ObsViewItem) element2;
        element.dispose();
      }
      m_items.clear();
    }

    fireObsViewChanged( new ObsViewEvent( this, ObsViewEvent.TYPE_ITEM_REMOVE_ALL ) );
  }

  public final void addItem( final ObsViewItem item )
  {
    synchronized( m_items )
    {
      m_items.add( item );
    }

    fireObsViewChanged( new ObsViewEvent( item, ObsViewEvent.TYPE_ITEM_ADD ) );
  }

  public final void removeItem( final ObsViewItem item )
  {
    synchronized( m_items )
    {
      m_items.remove( item );
      item.dispose();
    }

    fireObsViewChanged( new ObsViewEvent( item, ObsViewEvent.TYPE_ITEM_REMOVE ) );
  }

  public ObsViewItem[] getItems( )
  {
    synchronized( m_items )
    {
      return m_items.toArray( new ObsViewItem[m_items.size()] );
    }
  }

  public final void refreshView( final Object source )
  {
    fireObsViewChanged( new ObsViewEvent( source, this, ObsViewEvent.TYPE_VIEW_CHANGED ) );
  }

  public final void refreshItemData( final ObsViewItem item, final Object source )
  {
    fireObsViewChanged( new ObsViewEvent( source, item, ObsViewEvent.TYPE_ITEM_DATA_CHANGED ) );
  }

  public final void refreshItemState( final ObsViewItem item, final Object source )
  {
    fireObsViewChanged( new ObsViewEvent( source, item, ObsViewEvent.TYPE_ITEM_STATE_CHANGED ) );
  }

  public void addObsViewEventListener( final IObsViewEventListener l )
  {
    synchronized( m_listeners )
    {
      m_listeners.add( l );
    }
  }

  public void removeObsViewListener( final IObsViewEventListener l )
  {
    synchronized( m_listeners )
    {
      m_listeners.remove( l );
    }
  }

  protected void fireObsViewChanged( final ObsViewEvent evt )
  {
    synchronized( m_listeners )
    {
      final Object[] listeners = m_listeners.toArray();
      for( final Object element : listeners )
        ((IObsViewEventListener) element).onObsViewChanged( evt );
    }
  }

  protected void firePrintObsView( final ObsViewEvent evt )
  {
    synchronized( m_listeners )
    {
      final Object[] listeners = m_listeners.toArray();
      for( final Object element : listeners )
        ((IObsViewEventListener) element).onPrintObsView( evt );
    }
  }

  /**
   * Load an observation asynchronuously.
   */
  public IStatus loadObservation( final URL context, final String href, final boolean ignoreExceptions, final String tokenizedName, final ItemData data )
  {
    return loadObservation( context, href, ignoreExceptions, tokenizedName, data, false );
  }

  /**
   * Loads an observation, if synchro is true, the load is performed synchronuously.
   */
  public IStatus loadObservation( final URL context, final String href, final boolean ignoreExceptions, final String tokenizedName, final ItemData data, final boolean synchron )
  {
    final PoolableObjectType k = new PoolableObjectType( "zml", href, context, ignoreExceptions );

    final PoolableObjectWaiter waiter = new PoolableObjectWaiter( k, new Object[] { this, data, tokenizedName }, synchron )
    {
      @Override
      protected void objectLoaded( final IPoolableObjectType key, final Object newValue )
      {
        final IObsProvider provider = new PooledObsProvider( key, null );
        try
        {
          ((ObsView) m_data[0]).addObservation( provider, (String) m_data[2], (ObsView.ItemData) m_data[1] );
        }
        finally
        {
          provider.dispose();
        }
      }
    };

    return waiter.getResult();
  }

  /**
   * Implementors of this class must ensure, that there is a 1:1 relationship between provider and added item. So the
   * given provider should be disposed and for each added item a copy of the given provider should be made.
   */
  protected abstract void addObservation( final IObsProvider provider, final String tokenizedName, final ItemData data );

  public static Map<IObservation, ArrayList<ObsViewItem>> mapItems( final ObsViewItem[] items )
  {
    // obs -> columns
    final Map<IObservation, ArrayList<ObsViewItem>> obsmap = new HashMap<IObservation, ArrayList<ObsViewItem>>();
    for( final ObsViewItem element : items )
    {
      final IObservation observation = element.getObservation();
      if( !obsmap.containsKey( observation ) )
        obsmap.put( observation, new ArrayList<ObsViewItem>() );

      ((List<ObsViewItem>) obsmap.get( observation )).add( element );
    }

    return obsmap;
  }

  /**
   * Sets the given feature as enabled or not
   */
  public void setFeatureEnabled( final String featureName, final boolean enabled )
  {
    if( enabled )
      m_enabledFeatures.add( featureName );
    else
      m_enabledFeatures.remove( featureName );

    fireObsViewChanged( new ObsViewEvent( this, ObsViewEvent.TYPE_FEATURES_CHANGED ) );
  }

  /**
   * @return true if the given feature is enabled in this view
   */
  public boolean isFeatureEnabled( final String featureName )
  {
    return m_enabledFeatures.contains( featureName );
  }

  /**
   * @return the list of enabled features
   */
  public String[] getEnabledFeatures( )
  {
    return m_enabledFeatures.toArray( new String[m_enabledFeatures.size()] );
  }

  /**
   * Clears all enabled features
   */
  public void clearFeatures( )
  {
    m_enabledFeatures.clear();
  }

  public List<String> getIgnoreTypesAsList( )
  {
    final String[] ignoreTypes = getIgnoreTypes();
    final List<String> ignoreTypeList = ignoreTypes == null ? new ArrayList<String>() : Arrays.asList( ignoreTypes );
    return Collections.unmodifiableList( ignoreTypeList );
  }

  /**
   * Hide items which are displaying an observation which axis is of the given type. This method is the pendant to
   * setIgnoreType, but in contrario to the former, it only hides the items in the ui (it does not remove it).
   * 
   * @param types
   *            list of types that should be hidden
   */
  public void hideTypes( final List types )
  {
    if( types == null )
      m_hiddenTypes = new ArrayList();
    else
      m_hiddenTypes = types;

    final ObsViewItem[] items = getItems();
    for( int i = 0; i < items.length; i++ )
      items[i].setShown( !items[i].shouldBeHidden( m_hiddenTypes ) );
  }

  /**
   * @return the list of hidden types
   */
  public List getHiddenTypes( )
  {
    return m_hiddenTypes;
  }

  public void setTimezone( final TimeZone timezone )
  {
    m_timezone = timezone;

    fireObsViewChanged( new ObsViewEvent( this, ObsViewEvent.TYPE_VIEW_CHANGED ) );
  }

  public TimeZone getTimezone( )
  {
    return m_timezone;
  }
}

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
package org.kalypso.ogc.sensor.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.request.IRequest;

/**
 * Default implementation of the <code>IObservation</code> interface.
 * 
 * @author schlienger
 */
public class SimpleObservation implements IObservation
{
  private String m_name;

  private boolean m_editable;

  private Object m_target;

  private MetadataList m_metadata;

  private IAxis[] m_axes;

  private ITuppleModel m_tupples = null;

  private String m_identifier;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter( this );

  private String m_href;

  public SimpleObservation( )
  {
    this( "", "", "", false, null, new MetadataList(), new IAxis[0] );
  }

  public SimpleObservation( final IAxis[] axes )
  {
    this( "", "", "", false, null, new MetadataList(), axes );
  }

  public SimpleObservation( final String href, final String identifier, final String name, final boolean editable, final Object target, final MetadataList metadata, final IAxis[] axes )
  {
    this( href, identifier, name, editable, target, metadata, axes, new SimpleTuppleModel( axes ) );
  }

  public SimpleObservation( final String href, final String identifier, final String name, final boolean editable, final Object target, final MetadataList metadata, final IAxis[] axes, final ITuppleModel model )
  {
    m_href = href;
    m_identifier = identifier;
    m_name = name;
    m_editable = editable;
    m_target = target;
    m_metadata = metadata;
    m_axes = axes;
    m_tupples = model;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
  {
    return m_editable;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public Object getTarget( )
  {
    return m_target;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList( )
  {
    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
  public ITuppleModel getValues( final IRequest request ) throws SensorException
  {
    if( m_tupples == null )
      throw new SensorException( "Keine Werte vorhanden." );

    // TODO this leads to unsaved changes when a value is set because the underlying
    // (real) model isn't changed, just the copy of it (see setFrom and the calling
    // constructors in SimpleTuppleModel).
    if( request != null && request.getDateRange() != null )
      return new SimpleTuppleModel( m_tupples, request.getDateRange() );

    return m_tupples;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    if( values == null )
    {
      m_tupples = null;
      return;
    }

    if( m_tupples == null )
    {
      m_tupples = values;
      return;
    }

    final IAxis[] otherAxes = values.getAxisList();
    final Map<IAxis, IAxis> map = new HashMap<IAxis, IAxis>( m_axes.length );

    for( int i = 0; i < m_axes.length; i++ )
    {
      final IAxis myA = m_axes[i];

      try
      {
        final IAxis A = ObservationUtilities.findAxisByName( otherAxes, myA.getName() );

        map.put( myA, A );
      }
      catch( NoSuchElementException e )
      {
        throw new SensorException( "Values Models are not compatible. Current Observation: " + this.toString(), e );
      }
    }

    final IAxis[] keys = ObservationUtilities.findAxesByKey( m_axes );

    for( int i = 0; i < values.getCount(); i++ )
    {
      // check presence of values if associated axes are keys
      int ixPresent = -1;

      for( int j = 0; j < keys.length; j++ )
      {
        final Object obj = values.getElement( i, map.get( keys[j] ) );
        final int ix = m_tupples.indexOf( obj, keys[j] );

        if( ix >= 0 && ixPresent != -1 )
        {
          if( ixPresent != ix )
            break;
        }
        else
          ixPresent = ix;
      }

      // replace if values of keys already exist
      if( ixPresent != -1 )
      {
        final Set kset = map.keySet();

        for( final Iterator it = kset.iterator(); it.hasNext(); )
        {
          final IAxis myA = (IAxis) it.next();
          final IAxis oA = map.get( myA );

          final Object obj = values.getElement( i, oA );
          m_tupples.setElement( ixPresent, obj, myA );
        }
      }
      else
      {
        final Set kset = map.keySet();

        final Object[] tupple = new Object[kset.size()];

        final SimpleTuppleModel stm = prepareForAdding();

        for( final Iterator it = kset.iterator(); it.hasNext(); )
        {
          final IAxis myA = (IAxis) it.next();

          final Object obj = values.getElement( i, map.get( myA ) );
          tupple[stm.getPositionFor( myA )] = obj;
        }

        stm.addTupple( tupple );
      }
    }

    m_evtPrv.fireChangedEvent( null );
  }

  /**
   * Helper: since we are adding tupples to our model, we need a way to be sure that this is possible. For now, we
   * simply copy the existing values in a SimpleTuppleModel which finally allows to add tupples as desired.
   * 
   * @return a SimpleTuppleModel
   * @throws SensorException
   */
  private SimpleTuppleModel prepareForAdding( ) throws SensorException
  {
    // since we are adding
    if( !(m_tupples instanceof SimpleTuppleModel) )
      m_tupples = new SimpleTuppleModel( m_tupples );

    return (SimpleTuppleModel) m_tupples;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_identifier;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#addListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void addListener( IObservationListener listener )
  {
    m_evtPrv.addListener( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#removeListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void removeListener( IObservationListener listener )
  {
    m_evtPrv.removeListener( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#clearListeners()
   */
  public void clearListeners( )
  {
    m_evtPrv.clearListeners();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#fireChangedEvent(java.lang.Object)
   */
  public void fireChangedEvent( final Object source )
  {
    m_evtPrv.fireChangedEvent( source );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    return m_href;
  }

  /**
   * Sets the href
   * 
   * @param href
   *          localisation of the observation when it comes from a zml file for instance.
   */
  public void setHref( final String href )
  {
    m_href = href;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Obs: " + m_name + " - Id:" + m_identifier + " - Href:" + m_href;
  }
}
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
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * Default implementation of the <code>IObservation</code> interface.
 * 
 * @author schlienger
 */
public class SimpleObservation implements IObservation
{
  private String m_name;

  private boolean m_editable;

  private IXlink m_target;

  private MetadataList m_metadata;

  private IAxis[] m_axes;

  private ITuppleModel m_tupples = null;

  private String m_identifier;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter( this );

  private String m_href;

  /**
   * Default constructor
   */
  public SimpleObservation()
  {
    this( "", "", "", false, null, new MetadataList(), new IAxis[0] );
  }

  public SimpleObservation( final String href, final String identifier, final String name, final boolean editable,
      final IXlink target, final MetadataList metadata, final IAxis[] axes )
  {
    this( href, identifier, name, editable, target, metadata, axes, new SimpleTuppleModel( axes ) );
  }
  
  public SimpleObservation( final String href, final String identifier, final String name, final boolean editable,
      final IXlink target, final MetadataList metadata, final IAxis[] axes, final ITuppleModel model )
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
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    return m_editable;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget()
  {
    return m_target;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList()
  {
    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args ) throws SensorException
  {
    if( m_tupples == null )
      throw new SensorException( "Keine Werte vorhanden." );

    if( args instanceof DateRangeArgument )
      return new SimpleTuppleModel( m_tupples, (DateRangeArgument)args );
    
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

    // TODO: commented this test out because the gui might add the status axis when not
    // already available, thus leading to one more axis.
    // even if this is the case, this additional axis won't be added to this observation
    // since we only take the axes that are already present in this observation.
    //    if( m_axes.length != otherAxes.length )
    // throw new SensorException( "Not same amount of Axes" );

    final Map map = new HashMap( m_axes.length );

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

    final IAxis[] keys = ObservationUtilities.findAxisByKey( m_axes );

    for( int i = 0; i < values.getCount(); i++ )
    {
      // check presence of values if associated axes are keys
      int ixPresent = -1;

      for( int j = 0; j < keys.length; j++ )
      {
        final Object obj = values.getElement( i, (IAxis)map.get( keys[j] ) );
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
          final IAxis myA = (IAxis)it.next();
          final IAxis oA = (IAxis)map.get( myA );
          
          final Object obj = values.getElement( i, oA );
          m_tupples.setElement( ixPresent, obj, myA );
        }
      }
      else
      {
        final Set kset = map.keySet();

        final Object[] tupple = new Object[ kset.size() ];
        
        final SimpleTuppleModel stm = prepareForAdding();
        
        for( final Iterator it = kset.iterator(); it.hasNext(); )
        {
          final IAxis myA = (IAxis)it.next();

          final Object obj = values.getElement( i, (IAxis)map.get( myA ) );
          tupple[ stm.getPositionFor( myA ) ] = obj;
        }
        
        stm.addTupple( tupple );
      }
    }
    
    m_evtPrv.fireChangedEvent();
  }

  /**
   * Helper: since we are adding tupples to our model, we need a way to be sure that this
   * is possible. For now, we simply copy the existing values in a SimpleTuppleModel
   * which finally allows to add tupples as desired.
   * 
   * @return a SimpleTuppleModel
   * 
   * @throws SensorException
   */
  private SimpleTuppleModel prepareForAdding() throws SensorException
  {
    // since we are adding
    if( !( m_tupples instanceof SimpleTuppleModel ) )
      m_tupples = new SimpleTuppleModel( m_tupples );
    
    return (SimpleTuppleModel)m_tupples;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
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
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    return m_href;
  }
  
  /**
   * Sets the href
   * 
   * @param href localisation of the observation when it comes from a zml file for instance.
   */
  public void setHref( final String href )
  {
    m_href = href;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return "Obs: " + m_name + " - Id:" + m_identifier + " - Href:" + m_href;
  }
}
package org.kalypso.ogc.sensor.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
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

  /**
   * Default constructor
   */
  public SimpleObservation()
  {
    this( "", "", false, null, new MetadataList(), new IAxis[0] );
  }

  public SimpleObservation( final String identifier, final String name, final boolean editable,
      final IXlink target, final MetadataList metadata, final IAxis[] axes )
  {
    this( identifier, name, editable, target, metadata, axes, new SimpleTuppleModel( axes ) );
  }
  
  public SimpleObservation( final String identifier, final String name, final boolean editable,
      final IXlink target, final MetadataList metadata, final IAxis[] axes, final ITuppleModel model )
  {
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
   * @param args
   *          not used here
   * 
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args ) throws SensorException
  {
    if( m_tupples == null )
      throw new SensorException( "Keine Werte vorhanden." );

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

    if( m_axes.length != otherAxes.length )
      throw new SensorException( "Axes do not have same length" );

    final Map map = new HashMap( m_axes.length );

    for( int i = 0; i < m_axes.length; i++ )
    {
      final IAxis myA = m_axes[i];

      try
      {
        final IAxis A = ObservationUtilities.findAxis( otherAxes, myA.getLabel() );

        map.put( myA, A );
      }
      catch( NoSuchElementException e )
      {
        throw new SensorException( "Values Models are not compatible", e );
      }
    }

    final IAxis[] keys = ObservationUtilities.extractKeyAxis( m_axes );

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

          final Object obj = values.getElement( i, (IAxis)map.get( myA ) );
          m_tupples.setElement( ixPresent, obj, myA );
        }
      }
      else
      {
        final Set kset = map.keySet();

        final Object[] tupple = new Object[ kset.size() ];
        
        for( final Iterator it = kset.iterator(); it.hasNext(); )
        {
          final IAxis myA = (IAxis)it.next();

          final Object obj = values.getElement( i, (IAxis)map.get( myA ) );
          tupple[ myA.getPosition() ] = obj;
        }
        
        final SimpleTuppleModel stm = prepareForAdding();
        stm.addTupple( tupple );
      }
    }
  }

  /**
   * Helper: since we are adding tupples to our model, we need a way to be sure that this
   * is feasible. For now, we simply copy the existing values in a SimpleTuppleModel
   * which finally allows to add tupples as desired.
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
}
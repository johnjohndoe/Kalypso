package org.kalypso.ogc.sensor.impl;

import javax.swing.table.DefaultTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * <code>DefaultTableModel</code> based implementation of the
 * <code>ITuppleModel</code> interface.
 * 
 * @author schlienger
 */
public class SimpleTuppleModel implements ITuppleModel
{
  /** values are backed by this table model */
  private DefaultTableModel m_tupples;

  /** axes used within this model */
  private final IAxis[] m_axes;

  /**
   * Constructor with axes, empty data
   * @param axes
   */
  public SimpleTuppleModel( final IAxis[] axes )
  {
    this( axes, new Object[0][axes.length] );
  }

  /**
   * Constructor with data
   * @param axes
   * @param values
   */
  public SimpleTuppleModel( final IAxis[] axes, final Object[][] values )
  {
    m_tupples = new DefaultTableModel( values, axes );
    m_axes = axes;
  }

  /**
   * Constructor with model. A <code>DefaultTableModel</code> is used to back
   * the values which are taken from the given model.
   * @param copyTupples
   * @throws SensorException
   */
  public SimpleTuppleModel( final ITuppleModel copyTupples ) throws SensorException
  {
    m_axes = copyTupples.getAxisList();

    setFrom( copyTupples );
  }

  /**
   * A <code>DefaultTableModel</code> is used to back the values which are
   * taken from the given model.
   * @param copyTupples
   * @throws SensorException
   */
  public final void setFrom( final ITuppleModel copyTupples ) throws SensorException
  {
    m_tupples = new DefaultTableModel( copyTupples.getCount(), m_axes.length );

    for( int ix = 0; ix < copyTupples.getCount(); ix++ )
    {
      for( int i = 0; i < m_axes.length; i++ )
      {
        final Object element = copyTupples.getElement( ix, m_axes[i] );

        m_tupples.setValueAt( element, ix, m_axes[i].getPosition() );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    return m_tupples.getRowCount();
  }

  /**
   * Adds a tupple at the end of the model.
   * 
   * @param tupple
   *          the 'row' to be added
   */
  public void addTupple( final Object[] tupple )
  {
    m_tupples.addRow( tupple );
  }
  
  /**
   * Inserts the tupple at given position.
   * 
   * @param index
   * @param tupple
   */
  public void insertTupple( final int index, final Object[] tupple )
  {
    m_tupples.insertRow( index, tupple );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis )
  {
    for( int i = 0; i < m_tupples.getRowCount(); i++ )
    {
      if( m_tupples.getValueAt( i, axis.getPosition() ).equals( element ) )
        return i;
    }

    return -1;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis )
  {
    return m_tupples.getValueAt( index, axis.getPosition() );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element, final IAxis axis )
  {
    m_tupples.setValueAt( element, index, axis.getPosition() );
  }
}
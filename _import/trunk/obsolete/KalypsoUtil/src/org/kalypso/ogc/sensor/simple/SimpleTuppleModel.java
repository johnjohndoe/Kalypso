package org.kalypso.ogc.sensor.simple;

import javax.swing.table.DefaultTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;

/**
 * @author schlienger
 */
public class SimpleTuppleModel implements ITuppleModel
{
  private DefaultTableModel m_tupples;

  public SimpleTuppleModel( final IAxis[] axes )
  {
    this( axes, new Object[0][axes.length] );
  }

  public SimpleTuppleModel( final IAxis[] axes, final Object[][] values )
  {
    if( values == null )
      throw new IllegalArgumentException( "null values" );

    m_tupples = new DefaultTableModel( values, axes );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    return m_tupples.getRowCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, int)
   */
  public Object getElement( int index, int position )
  {
    return m_tupples.getValueAt( index, position );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      int)
   */
  public void setElement( int index, Object element, int position )
  {
    m_tupples.setValueAt( element, index, position );
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
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis )
  {
    for( int i = 0; i < m_tupples.getRowCount(); i++ )
    {
      if( m_tupples.getValueAt( i, axis.getPosition() ) == element )
        return i;
    }

    return -1;
  }
}
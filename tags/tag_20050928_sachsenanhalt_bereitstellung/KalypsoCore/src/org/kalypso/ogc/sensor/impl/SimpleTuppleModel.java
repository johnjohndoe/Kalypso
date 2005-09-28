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

import java.util.Date;
import java.util.List;
import java.util.Vector;
import java.util.logging.Logger;

import javax.swing.table.DefaultTableModel;

import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;

/**
 * <code>DefaultTableModel</code> based implementation of the <code>ITuppleModel</code> interface.
 * 
 * @author schlienger
 */
public class SimpleTuppleModel extends AbstractTuppleModel
{
  public final static ITuppleModel EMPTY_TUPPLEMODEL = new SimpleTuppleModel( new IAxis[0] );

  /** values are backed by this table model */
  private DefaultTableModel m_tupples;

  /**
   * Constructor with axes, empty data
   */
  public SimpleTuppleModel( final List axes )
  {
    this( (IAxis[])axes.toArray( new IAxis[axes.size()] ) );
  }

  /**
   * Constructor with axes, empty data
   */
  public SimpleTuppleModel( final IAxis[] axes )
  {
    this( axes, new Object[0][axes.length] );
  }

  /**
   * Constructor with model. A <code>DefaultTableModel</code> is used to back the values which are taken from the
   * given model.
   */
  public SimpleTuppleModel( final ITuppleModel copyTupples ) throws SensorException
  {
    this( copyTupples.getAxisList() );

    // TODO this leads to unsaved changes when a value is set because the underlying
    // (real) model isn't changed, just the copy of it (see setFrom and the calling
    // constructors in SimpleTuppleModel).
    setFrom( copyTupples );
  }

  /**
   * Constructor with model. A <code>DefaultTableModel</code> is used to back the values which are taken from the
   * given model. The additional DateRangeArgument is used to limit the values that are returned by this model.
   */
  public SimpleTuppleModel( final ITuppleModel tupples, final DateRange dra ) throws SensorException
  {
    this( tupples.getAxisList() );

    // TODO this leads to unsaved changes when a value is set because the underlying
    // (real) model isn't changed, just the copy of it (see setFrom and the calling
    // constructors in SimpleTuppleModel).
    setFrom( tupples, dra );
  }

  /**
   * Constructor with data
   */
  public SimpleTuppleModel( final IAxis[] axes, final Object[][] values )
  {
    super( axes );

    m_tupples = new DefaultTableModel( values, axes );
  }

  /**
   * A <code>DefaultTableModel</code> is used to back the values which are taken from the given model.
   */
  public final void setFrom( final ITuppleModel copyTupples ) throws SensorException
  {
    IAxis[] axes = getAxisList();

    m_tupples = new DefaultTableModel( copyTupples.getCount(), axes.length );

    clearAxesPositions();
    for( int ia = 0; ia < axes.length; ia++ )
      mapAxisToPos( axes[ia], ia );

    for( int ix = 0; ix < copyTupples.getCount(); ix++ )
    {
      for( int ia = 0; ia < axes.length; ia++ )
      {
        final Object element = copyTupples.getElement( ix, axes[ia] );

        m_tupples.setValueAt( element, ix, ia );
      }
    }
  }

  /**
   * A <code>DefaultTableModel</code> is used to back the values which are taken from the given model. Uses the given
   * <code>DateRangeArgument</code> to check which values to copy.
   */
  public final void setFrom( final ITuppleModel copyTupples, final DateRange dra ) throws SensorException
  {
    IAxis[] axes = getAxisList();

    final IAxis dateAxis = ObservationUtilities.findAxisByClassNoEx( axes, Date.class );
    if( dra == null || dateAxis == null )
    {
      setFrom( copyTupples );
      return;
    }

    // uses same row count as original model, adjusted before method finishes
    m_tupples = new DefaultTableModel( copyTupples.getCount(), axes.length );

    clearAxesPositions();
    for( int ia = 0; ia < axes.length; ia++ )
      mapAxisToPos( axes[ia], ia );

    int realIx = 0;

    for( int ix = 0; ix < copyTupples.getCount(); ix++ )
    {
      final Date d = (Date)copyTupples.getElement( ix, dateAxis );

      if( d.compareTo( dra.getFrom() ) >= 0 && d.compareTo( dra.getTo() ) <= 0 )
      {
        for( int ia = 0; ia < axes.length; ia++ )
        {
          final Object element = copyTupples.getElement( ix, axes[ia] );

          m_tupples.setValueAt( element, realIx, ia );
        }

        realIx++;
      }
    }

    // readjust row count according to real amount of rows
    m_tupples.setRowCount( realIx );
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
   * Adds a tupple at the end of the model
   */
  public void addTupple( final Vector tupple )
  {
    m_tupples.addRow( tupple );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException
  {
    if( element == null )
      return -1;

    for( int i = 0; i < m_tupples.getRowCount(); i++ )
    {
      if( element.equals( m_tupples.getValueAt( i, getPositionFor( axis ) ) ) )
        return i;
    }

    return -1;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException
  {
    final Object value = m_tupples.getValueAt( index, getPositionFor( axis ) );
    
    return value;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element, final IAxis axis ) throws SensorException
  {
    // for debug purposes! once problem with "null" is solved remove?
    if( element == null )
      Logger.getLogger( SimpleTuppleModel.class.getName() ).warning(
          "Setting a null element at index= " + index + " for axis " + axis );

    m_tupples.setValueAt( element, index, getPositionFor( axis ) );
  }
}
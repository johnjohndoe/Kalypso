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
package org.kalypso.ogc.sensor.tableview.swing.editor;

import java.awt.Color;
import java.awt.Component;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;

import javax.swing.DefaultCellEditor;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.LineBorder;

/**
 * DoubleCellEditor, uses a NumberFormat that you can configure according to your needs. In the constructor, there is
 * also the possibility to force the use of the grouping symbol as decimal symbol. In that case, both decimal and
 * grouping symbols will denote a decimal symbol.
 * 
 * @author schlienger
 */
public class DoubleCellEditor extends DefaultCellEditor
{
  private Object m_value;

  private final NumberFormat m_nf;

  private final boolean m_useGroupingAsDecimalSymbol;

  private char m_replaceWhat = ' ';

  private char m_replaceWith = ' ';

  private final Double m_replacementWhenNull;

  /**
   * Constructor. If useGroupingAsDecimalSymbol is true and nf is a DecimalFormat, it tries to fetch the grouping and
   * decimal symbols to be replaced.
   * 
   * @param nf
   * @param useGroupingAsDecimalSymbol
   * @param replacementWhenNull
   *          [optional, ignored if null] if specified, null values are not allowed and they are replaced by this number
   */
  public DoubleCellEditor( final NumberFormat nf, final boolean useGroupingAsDecimalSymbol,
      final Double replacementWhenNull )
  {
    super( new JTextField() );

    m_nf = nf;
    m_useGroupingAsDecimalSymbol = useGroupingAsDecimalSymbol;
    m_replacementWhenNull = replacementWhenNull;

    if( useGroupingAsDecimalSymbol && nf instanceof DecimalFormat )
    {
      final DecimalFormat df = (DecimalFormat)nf;
      final DecimalFormatSymbols bols = df.getDecimalFormatSymbols();

      m_replaceWhat = bols.getGroupingSeparator();
      m_replaceWith = bols.getDecimalSeparator();
    }

    ( (JTextField)getComponent() ).setHorizontalAlignment( SwingConstants.RIGHT );
  }

  @Override
  public Component getTableCellEditorComponent( JTable table, Object value, boolean isSelected, int row, int column )
  {
    m_value = null;
    ( (JComponent)getComponent() ).setBorder( new LineBorder( Color.black ) );

    try
    {
      value = m_nf.format( value );
    }
    catch( IllegalArgumentException e )
    {
      // ignored
    }

    return super.getTableCellEditorComponent( table, value, isSelected, row, column );
  }

  @Override
  public boolean stopCellEditing()
  {
    String s = (String)super.getCellEditorValue();

    if( s == null || "".equals( s ) ) //$NON-NLS-1$
    {
      m_value = m_replacementWhenNull != null ? m_replacementWhenNull : null;
      super.stopCellEditing();
    }
    else
    {
      if( m_useGroupingAsDecimalSymbol )
        s = s.replace( m_replaceWhat, m_replaceWith );
    }

    try
    {
      m_value = new Double( m_nf.parse( s ).doubleValue() );
    }
    catch( Exception e )
    {
      ( (JComponent)getComponent() ).setBorder( new LineBorder( Color.red ) );
      return false;
    }

    return super.stopCellEditing();
  }

  /**
   * Overriden to set the editor's value to null. This is used as an indication that editing has been cancelled. Our
   * ObservationTableModel will ignore this value and won't change the status associated to it.
   * 
   * @see javax.swing.DefaultCellEditor#cancelCellEditing()
   */
  @Override
  public void cancelCellEditing()
  {
    super.cancelCellEditing();

    m_value = null;
  }

  @Override
  public Object getCellEditorValue( )
  {
    return m_value;
  }
}
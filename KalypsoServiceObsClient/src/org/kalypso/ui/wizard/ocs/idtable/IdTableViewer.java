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
package org.kalypso.ui.wizard.ocs.idtable;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;

/**
 * IdTableViewer
 * 
 * @author schlienger
 */
public class IdTableViewer extends DefaultTableViewer implements ICellModifier
{
  private static final String COL_HREF = "COL_HREF";

  private static final String COL_OBS = "COL_OBS";

  public IdTableViewer( Composite parent, int style )
  {
    super( parent, style );

    setContentProvider( new IdTableContentProvider() );
    setLabelProvider( new IdTableLabelProvider() );
    setCellEditors( new CellEditor[]
    {
        null,
        new ObservationCellEditor( parent ) } );
    setCellModifier( this );

    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    addColumn( COL_OBS, "Zeitreihe", 250, true, SWT.CENTER );
    addColumn( COL_HREF, "Kennzeichen", 400, true, SWT.CENTER );

    //refreshColumnProperties();
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( Object element, String property )
  {
    return property.equals( COL_HREF );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( Object element, String property )
  {
    if( property.equals( COL_HREF ) )
      return ( (IdStruct)element ).getId();

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( Object element, String property, Object value )
  {
    if( property.equals( COL_HREF ) )
    {
      final Object data = ( (Item)element ).getData();
      final IdStruct ids = (IdStruct)data;
      ids.setId( (String)value );
    }
  }
}
/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.om.table.command;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

/**
 * Copies the contents of a (tuple-result) table into the clipboard.
 * 
 * @author Gernot Belger
 */
public class CopyToClipboardHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final TableViewer tupleResultViewer = TupleResultCommandUtils.findTableViewer( event );
    if( tupleResultViewer == null )
      throw new ExecutionException( "No tuple result viewer available" );

    /* clip board copy function */
    final Table table = tupleResultViewer.getTable();

    // if there is nothing selected, select all
    final boolean fullSelection = table.getSelectionCount() == 0;

    final StringBuffer sbf = new StringBuffer();

    final int columnCount = table.getColumnCount();

    // walk through every row (item)
    for( int i = 0; i < table.getItemCount(); i++ )
    {
      if( fullSelection || table.isSelected( i ) )
      {
        final TableItem item = table.getItem( i );

        // REMARK: we start from 1 because there exists a invisible column 0 which has no data.
        for( int j = 1; j < columnCount; j++ )
        {
          final String text = item.getText( j );
          sbf.append( text );

          if( j < columnCount - 1 )
            sbf.append( "\t" );
        }
        sbf.append( "\n" );
      }
    }

    final StringSelection stsel = new StringSelection( sbf.toString() );
    Toolkit.getDefaultToolkit().getSystemClipboard().setContents( stsel, stsel );

    return null;
  }

}

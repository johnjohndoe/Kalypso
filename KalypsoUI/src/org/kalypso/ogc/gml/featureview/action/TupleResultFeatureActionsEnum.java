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
package org.kalypso.ogc.gml.featureview.action;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.ui.ImageProvider;

/**
 * @author Thomas Jung
 */
public enum TupleResultFeatureActionsEnum
{
  COPY("Kopieren", "In Zwischenablage kopieren", ImageProvider.IMAGE_TABLE_COPY, null, IAction.AS_PUSH_BUTTON)
  {
    /**
     * @see org.kalypso.ogc.gml.featureview.action.TupleResultFeatureActionsEnum#run(org.eclipse.jface.viewers.TableViewer)
     */
    @Override
    protected void run( TableViewer tupleResultViewer )
    {
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

    }
  };

  private final int m_style;

  private final String m_tooltip;

  private final String m_label;

  private final ImageDescriptor m_enabledImage;

  private final ImageDescriptor m_disabledImage;

  private final String m_menuPath;

  TupleResultFeatureActionsEnum( final String label, final String tooltip, final ImageDescriptor enabledImage, final ImageDescriptor disabledImage, final int style )
  {
    this( label, tooltip, enabledImage, disabledImage, style, null );
  }

  TupleResultFeatureActionsEnum( final String label, final String tooltip, final ImageDescriptor enabledImage, final ImageDescriptor disabledImage, final int style, final String menuPath )
  {
    m_label = label;
    m_tooltip = tooltip;
    m_enabledImage = enabledImage;
    m_disabledImage = disabledImage;
    m_style = style;
    m_menuPath = menuPath;
  }

  public static IAction createAction( final TableViewer tupleResultViewer, final TupleResultFeatureActionsEnum tupleResultAction )
  {
    final int style = tupleResultAction.getStyle();
    final String label = tupleResultAction.toString();

    final IAction action = new Action( label, style )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        tupleResultAction.run( tupleResultViewer );
      }
    };

    action.setToolTipText( tupleResultAction.getTooltip() );
    action.setImageDescriptor( tupleResultAction.getEnabledImage() );
    action.setDisabledImageDescriptor( tupleResultAction.getDisabledImage() );

    return action;
  }

  protected abstract void run( TableViewer tupleResultViewer );

  private ImageDescriptor getEnabledImage( )
  {
    return m_enabledImage;
  }

  private ImageDescriptor getDisabledImage( )
  {
    return m_disabledImage;
  }

  public int getStyle( )
  {
    return m_style;
  }

  public String getTooltip( )
  {
    return m_tooltip;
  }

}

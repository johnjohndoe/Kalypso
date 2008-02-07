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
package org.kalypso.ogc.gml.om.table;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiFirstColumnHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;

/**
 * @author Marc Schlienger
 */
public class TupleResultContentProvider implements IStructuredContentProvider, ITupleResultChangedListener
{
  private static final String DUMMY = "dummy";

  protected DefaultTableViewer m_tableViewer;

  private TupleResult m_result;

  private final IComponentUiHandlerProvider m_factory;

  private final Map<String, IComponentUiHandler> m_componentHandlers = new HashMap<String, IComponentUiHandler>();

  public TupleResultContentProvider( final IComponentUiHandlerProvider factory )
  {
    m_factory = factory;
  }

  /* default */
  static IComponentUiHandler[] addFakeHandler( final IComponentUiHandler[] componentHandlers )
  {
    final IComponentUiHandler[] handlerWithFake = new IComponentUiHandler[componentHandlers.length + 1];

    handlerWithFake[0] = new ComponentUiFirstColumnHandler();
    System.arraycopy( componentHandlers, 0, handlerWithFake, 1, componentHandlers.length );

    return handlerWithFake;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    final DefaultTableViewer tableViewer = (DefaultTableViewer) viewer;
    m_tableViewer = tableViewer;

    if( oldInput instanceof TupleResult )
      ((TupleResult) oldInput).removeChangeListener( this );

    m_result = (TupleResult) newInput;
    if( m_result != null )
    {
      // Only remove columns if input non null, because input==null may happen while disposing
      refreshColumns();
      m_result.addChangeListener( this );
    }
  }

  protected void refreshColumns( )
  {
    m_tableViewer.removeAllColumns();

    final Map<Integer, IComponentUiHandler> componentHandlers = m_factory.createComponentHandler( m_result );
    m_componentHandlers.clear();

    final List<CellEditor> cellEditors = new ArrayList<CellEditor>( m_componentHandlers.size() + 1 );

    // HACK: add a 'dummy' column (size 0) ,in order to avoid the windows feature, that the first column is always
    // left-aligned
    final ComponentUiFirstColumnHandler dummyHandler = new ComponentUiFirstColumnHandler();
    addColumn( DUMMY, dummyHandler );
    cellEditors.add( dummyHandler.createCellEditor( m_tableViewer.getTable() ) );
    m_componentHandlers.put( DUMMY, dummyHandler );

    for( final Entry<Integer, IComponentUiHandler> entry : componentHandlers.entrySet() )
    {
      final Integer componentIndex = entry.getKey();
      final IComponentUiHandler handler = entry.getValue();

      final String property = "" + componentIndex;

      addColumn( property, handler );

      cellEditors.add( handler.createCellEditor( m_tableViewer.getTable() ) );

      m_componentHandlers.put( property, handler );
    }

    m_tableViewer.setCellEditors( cellEditors.toArray( new CellEditor[cellEditors.size()] ) );
  }

  private void addColumn( final String property, final IComponentUiHandler handler )
  {
    final int columnWidth = handler.getColumnWidth();
    final int columnWidthPercent = handler.getColumnWidthPercent();
    final int columnStyle = handler.getColumnStyle();
    final boolean editable = handler.isEditable();
    final boolean resizeable = handler.isResizeable();
    final boolean moveable = handler.isMoveable();

    final String label = handler.getColumnLabel();

    final String tooltip = null;
    final TableColumn tc = m_tableViewer.addColumn( property, label, tooltip, columnWidth, columnWidthPercent, editable, columnStyle, resizeable, moveable );
    tc.setData( handler );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement != null && inputElement instanceof TupleResult )
    {
      final TupleResult result = (TupleResult) inputElement;

      return result.toArray();
    }

    return null;
  }

  public TupleResult getResult( )
  {
    return m_result;
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#valuesChanged(org.kalypso.observation.result.ITupleResultChangedListener.ValueChange[])
   */
  public void valuesChanged( final ValueChange[] changes )
  {
    if( m_result == null )
      return;

    final IRecord[] records = new IRecord[changes.length];
    final Set<String> properties = new HashSet<String>();
    for( int i = 0; i < changes.length; i++ )
    {
      final ValueChange change = changes[i];

      final IRecord record = change.getRecord();

      records[i] = record;

      final int compIndex = change.getComponent();
      properties.add( "" + compIndex );
    }

    final String[] props = properties.toArray( new String[properties.size()] );
    ViewerUtilities.update( m_tableViewer, records, props, true );
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#recordsChanged(org.kalypso.observation.result.IRecord[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void recordsChanged( final IRecord[] records, final TYPE type )
  {
    // TODO: Performance optimization needed for lots of single changes...

    final DefaultTableViewer tableViewer = m_tableViewer;
    final Control control = tableViewer.getControl();
    if( !control.isDisposed() )
    {
      control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !control.isDisposed() )
          {
            switch( type )
            {
              case ADDED:
                // TODO: optimize, depending on event (events must deliver more information)
                // we need the insert positions here... or the viewer should have an sorter?
// tableViewer.add( records );
// tableViewer.reveal( records[records.length - 1] );
                tableViewer.refresh();
                /* Total Refresh may shift the table, so current selection is no more visible */
                // TODO: how to fix this?
                break;

              case REMOVED:
                tableViewer.remove( records );
                break;

              case CHANGED:
              {
                if( records == null )
                {
                  tableViewer.refresh();
                  // REMARK: at this place it is OK to force the selection to be shown
                  // as it is quite probable that the user changed the value of the current selection
                  tableViewer.getTable().showSelection();
                }
                else
                  tableViewer.update( records, null );
              }
                break;
            }
          }
        }
      } );
    }
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#componentsChanged(org.kalypso.observation.result.IComponent[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void componentsChanged( final IComponent[] components, final TYPE type )
  {
    new UIJob( "updating table... " )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        refreshColumns();
        m_tableViewer.refresh();

        return Status.OK_STATUS;
      }
    }.schedule();
  }

  /**
   * Returns the handler for a given column property.
   */
  public IComponentUiHandler getHandler( final String property )
  {
    return m_componentHandlers.get( property );
  }

  /**
   * Returns the handler for a given column index.
   */
  public IComponentUiHandler getHandler( final int columnIndex )
  {
    final String property = m_tableViewer.getProperty( columnIndex );
    return getHandler( property );
  }

  // HACK: determine if the whole table is editable. This is the case, if one column is editable. Later, this
  // information should be set independently.
  public boolean isEditable( )
  {
    boolean editable = true;
    for( final IComponentUiHandler handler : m_componentHandlers.values() )
      editable |= handler.isEditable();
    return editable;
  }
}

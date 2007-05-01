/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.template.featureview.TupleResult.ColumnDescriptor;
import org.kalypso.util.swt.SWTUtilities;

/**
 * @author Marc Schlienger
 */
public class TupleResultContentProvider implements IStructuredContentProvider, ITupleResultChangedListener
{
  private final Map<String, IComponent> m_componentMap = new HashMap<String, IComponent>();

  private DefaultTableViewer m_tableViewer;

  private TupleResult m_result;

  private final Map<String, ColumnDescriptor> m_columnDescriptors;

  public TupleResultContentProvider( )
  {
    this( new HashMap<String, ColumnDescriptor>() );
  }

  public TupleResultContentProvider( final Map<String, ColumnDescriptor> columnDescriptors )
  {
    m_columnDescriptors = columnDescriptors;
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

    m_componentMap.clear();

    m_result = (TupleResult) newInput;
    if( m_result != null )
    {
      // Only remove columns if input non null, because input==null may happen while disposing
      refrehsColumns( m_result );
      m_result.addChangeListener( this );
    }
  }

  private void refrehsColumns( final TupleResult result )
  {
    m_tableViewer.removeAllColumns();

    final IComponent[] components = result == null ? new IComponent[] {} : result.getComponents();
    final List<CellEditor> cellEditors = new ArrayList<CellEditor>( components.length );
    for( final IComponent component : components )
    {
      final String id = component.getId();

      final int style = styleForComponent( component );

      m_tableViewer.addColumn( id, component.getName(), 100, true, style );
      m_componentMap.put( id, component );
      cellEditors.add( new TextCellEditor( m_tableViewer.getTable(), SWT.NONE )
      {
        /**
         * Overwritten in order to allow leaving of the cell-editor after editing via tab
         * 
         * @see org.eclipse.jface.viewers.TextCellEditor#createControl(org.eclipse.swt.widgets.Composite)
         */
        @Override
        protected Control createControl( final Composite parent )
        {
          final Control textControl = super.createControl( parent );

          text.addTraverseListener( new TraverseListener()
          {
            public void keyTraversed( TraverseEvent e )
            {
              if( e.detail == SWT.TRAVERSE_TAB_NEXT || e.detail == SWT.TRAVERSE_TAB_PREVIOUS )
                e.doit = false;
            }
          } );

          return textControl;
        }
      } );
    }

    m_tableViewer.refreshColumnProperties();
    m_tableViewer.setCellEditors( cellEditors.toArray( new CellEditor[cellEditors.size()] ) );
  }

  private int styleForComponent( final IComponent component )
  {
    final String id = component.getId();
    final ColumnDescriptor descriptor = m_columnDescriptors.get( id );
    if( descriptor == null )
      return SWT.CENTER;

    final String alignment = descriptor.getAlignment();
    if( alignment == null )
      return SWT.CENTER;

    final int style = SWTUtilities.createStyleFromString( alignment );
    if( style == SWT.NONE )
      return SWT.CENTER;

    return style;
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

  protected IComponent getComponent( final String property )
  {
    return m_componentMap.get( property );
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
    final IRecord[] records = new IRecord[changes.length];
    final IComponent[] components = m_result.getComponents();

    final Set<String> properties = new HashSet<String>();
    final Set<IRecord> emptyRecords = new HashSet<IRecord>();
    for( int i = 0; i < changes.length; i++ )
    {
      final ValueChange change = changes[i];

      final IRecord record = change.getRecord();

      records[i] = record;

      final String id = change.getComponent().getName();
      properties.add( id );

      /* check if we have an empty record */
      boolean isEmpty = true;
      for( final IComponent component : components )
      {
        final Object value = record.getValue( component );
        if( value != null )
        {
          isEmpty = false;
          break;
        }
      }

      if( isEmpty )
        emptyRecords.add( record );
    }

    if( emptyRecords.size() > 0 )
      m_result.removeAll( emptyRecords );
    else
    {
      final String[] props = properties.toArray( new String[properties.size()] );
      m_tableViewer.update( records, props );
    }
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#recordsChanged(org.kalypso.observation.result.IRecord[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void recordsChanged( final IRecord[] records, final TYPE type )
  {
    m_tableViewer.refresh();

    // switch( type )
    // {
    // case ADDED:
    // m_tableViewer.add( records );
    // break;
    //
    // case REMOVED:
    // m_tableViewer.remove( records );
    // break;
    // }
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#componentsChanged(org.kalypso.observation.result.IComponent[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void componentsChanged( final IComponent[] components, final TYPE type )
  {
    refrehsColumns( getResult() );
    m_tableViewer.refresh();
  }
  
  public Map<String, ColumnDescriptor> getColumnDescriptors( )
  {
    return m_columnDescriptors;
  }
}

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.editor.sldEditor;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;

/**
 * @author Thomas Jung creates a composite for a single {@link org.kalypsodeegree.graphics.sld.PolygonColorMapEntry}
 *         combining the composites of a stroke and a fill.
 */
public class PolygonColorMapEntryEditorComposite extends Composite
{
  private final Set<IPolygonColorMapEntryModifyListener> m_listeners = new HashSet<IPolygonColorMapEntryModifyListener>();

  private final PolygonColorMapEntry m_entry;

  public PolygonColorMapEntryEditorComposite( final Composite parent, final int style, final PolygonColorMapEntry entry )
  {
    super( parent, style );
    m_entry = entry;

    createControl();

  }

  private void createControl( )
  {
    setLayout( new GridLayout( 1, false ) );

    final StrokeEditorComposite strokeEditor = new StrokeEditorComposite( this, SWT.NONE, m_entry.getStroke(), false );
    strokeEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    strokeEditor.addModifyListener( new IStrokeModifyListener()
    {
      public void onStrokeChanged( Object source, Stroke stroke )
      {
        contentChanged();
      }
    } );

    final FillEditorComposite fillEditor = new FillEditorComposite( this, SWT.NONE, m_entry.getFill(), true );
    fillEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fillEditor.addModifyListener( new IFillModifyListener()
    {
      public void onFillChanged( Object source, Fill fill )
      {
        contentChanged();
      }
    } );
  }

  protected void disposeControl( )
  {
    m_listeners.clear();
  }

  /**
   * Add the listener to the list of listeners. If an identical listeners has already been registered, this has no
   * effect.
   */
  public void addModifyListener( final IPolygonColorMapEntryModifyListener l )
  {
    m_listeners.add( l );
  }

  public void removeModifyListener( final IPolygonColorMapEntryModifyListener l )
  {
    m_listeners.remove( l );
  }

  protected void fireModified( )
  {
    final IPolygonColorMapEntryModifyListener[] ls = m_listeners.toArray( new IPolygonColorMapEntryModifyListener[m_listeners.size()] );
    for( final IPolygonColorMapEntryModifyListener entryModifyListener : ls )
      entryModifyListener.onEntryChanged( this, m_entry );
  }

  protected void contentChanged( )
  {
    fireModified();
  }

}

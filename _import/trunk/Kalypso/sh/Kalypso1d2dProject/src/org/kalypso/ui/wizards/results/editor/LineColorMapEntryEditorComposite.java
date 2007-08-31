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
package org.kalypso.ui.wizards.results.editor;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ui.editor.sldEditor.IStrokeModifyListener;
import org.kalypso.ui.editor.sldEditor.StrokeEditorComposite;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;

/**
 * @author Thomas Jung
 * 
 * creates a composite for a single {@link org.kalypsodeegree.graphics.sld.PolygonColorMapEntry} combining the
 * composites of a stroke and a fill.
 * 
 */
public class LineColorMapEntryEditorComposite extends Composite
{
  private final Set<ILineColorMapEntryModifyListener> m_listeners = new HashSet<ILineColorMapEntryModifyListener>();

  private final LineColorMapEntry m_entry;

  public LineColorMapEntryEditorComposite( final Composite parent, final int style, final LineColorMapEntry entry )
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
  }

  protected void disposeControl( )
  {
    m_listeners.clear();
  }

  /**
   * Add the listener to the list of listeners. If an identical listeners has already been registered, this has no
   * effect.
   */
  public void addModifyListener( final ILineColorMapEntryModifyListener l )
  {
    m_listeners.add( l );
  }

  public void removeModifyListener( final ILineColorMapEntryModifyListener l )
  {
    m_listeners.remove( l );
  }

  protected void fireModified( )
  {
    final ILineColorMapEntryModifyListener[] ls = m_listeners.toArray( new ILineColorMapEntryModifyListener[m_listeners.size()] );
    for( final ILineColorMapEntryModifyListener entryModifyListener : ls )
      entryModifyListener.onEntryChanged( this, m_entry );
  }

  protected void contentChanged( )
  {
    fireModified();
  }

}

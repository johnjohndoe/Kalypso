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
package org.kalypso.ogc.gml.om.tableex;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.table.MultiTupleResultModel;
import org.kalypso.observation.table.TupleResultColumn;
import org.kalypso.ogc.gml.om.AbstractObservationView;

/**
 * @author schlienger
 */
public class MultiTupleResultView extends AbstractObservationView
{
  protected DefaultTableViewer m_viewer;

  protected final MultiTupleResultModel m_model = new MultiTupleResultModel();

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_viewer = new DefaultTableViewer( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    m_viewer.getTable().setHeaderVisible( true );
    m_viewer.getTable().setLinesVisible( true );
    addDropSupport( DND.DROP_COPY | DND.DROP_MOVE | DND.DROP_LINK, new Transfer[] { LocalSelectionTransfer.getInstance() } );

    final MultiTupleResultLabelProvider labelProvider = new MultiTupleResultLabelProvider();
    m_viewer.setContentProvider( new MultiTupleResultContentProvider( labelProvider ) );
    m_viewer.setLabelProvider( labelProvider );

    m_viewer.setInput( m_model );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    m_viewer.getControl().setFocus();
  }

  /**
   * @see org.kalypso.ogc.gml.om.views.AbstractObservationView#getControl()
   */
  @Override
  protected Control getControl( )
  {
    return m_viewer.getControl();
  }

  /**
   * @see org.kalypso.ogc.gml.om.views.AbstractObservationView#handleDrop(org.kalypso.observation.IObservation)
   */
  @Override
  protected boolean handleDrop( final IObservation<TupleResult> obs )
  {
    final IComponent[] components = obs.getResult().getComponents();

    if( m_model.getKeyComponent() == null && components.length >= 2 )
    {
      m_model.addColumn( new TupleResultColumn( components[1].getPosition(), obs.getResult(), components[0], components[1] ) );
      return true;
    }
    else if( m_model.getKeyComponent() != null )
    {
      final IComponent kc = ComponentUtilities.sameComponent( components, m_model.getKeyComponent() );
      final IComponent vc = ComponentUtilities.otherComponent( components, kc.getValueTypeName() );

      if( kc != null && vc != null )
      {
        m_model.addColumn( new TupleResultColumn( vc.getPosition(), obs.getResult(), kc, vc ) );
        return true;
      }
    }
    
    return false;
  }
}

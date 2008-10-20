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
package org.kalypso.ogc.gml.om;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author schlienger
 */
public abstract class AbstractObservationView extends ViewPart
{
  /**
   * Adds support for dropping items into this viewer via a user drag-and-drop operation.
   * 
   * @param operations
   *          a bitwise OR of the supported drag and drop operation types ( <code>DROP_COPY</code>,<code>DROP_LINK</code>,
   *          and <code>DROP_MOVE</code>)
   * @param transferTypes
   *          the transfer types that are supported by the drop operation
   * @see org.eclipse.swt.dnd.DND
   */
  public void addDropSupport( int operations, Transfer[] transferTypes )
  {
    final Control control = getControl();
    final DropTarget dropTarget = new DropTarget( control, operations );
    dropTarget.setTransfer( transferTypes );
    dropTarget.addDropListener( new DropAdapter() );
  }

  protected abstract Control getControl( );

  protected abstract boolean handleDrop( IObservation<TupleResult> obs );

  private class DropAdapter extends DropTargetAdapter
  {
    @Override
    public void drop( final DropTargetEvent event )
    {
      final ISelection selection = LocalSelectionTransfer.getInstance().getSelection();

      boolean success = false;

      if( selection instanceof IFeatureSelection )
      {
        final IFeatureSelection fSel = (IFeatureSelection) selection;
        final EasyFeatureWrapper[] features = fSel.getAllFeatures();

        for( int i = 0; i < features.length; i++ )
        {
          final Feature feature = features[i].getFeature();

          if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), ObservationFeatureFactory.OM_OBSERVATION ) )
          {
            final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( feature );

            success = handleDrop( obs );
          }
        }
      }

      if( !success )
        event.detail = DND.DROP_NONE;
    }
  }
}

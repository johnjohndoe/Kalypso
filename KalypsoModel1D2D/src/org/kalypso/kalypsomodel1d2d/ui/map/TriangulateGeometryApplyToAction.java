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
package org.kalypso.kalypsomodel1d2d.ui.map;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen.TriangulationBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class TriangulateGeometryApplyToAction extends Action
{
  private final TriangulationBuilder m_triangulationBuilder;

  private final CommandableWorkspace m_workspace;

  public TriangulateGeometryApplyToAction( final TriangulationBuilder triangulationBuilder, final CommandableWorkspace workspace )
  {
    m_triangulationBuilder = triangulationBuilder;
    m_workspace = workspace;
    setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.9" ) ); //$NON-NLS-1$
    setToolTipText( Messages.getString( "TriangulateGeometryApplyToAction.0" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.OK ) );

    final IObservableValue modelTin = BeansObservables.observeValue( triangulationBuilder, TriangulationBuilder.PROPERTY_TIN );
    modelTin.addValueChangeListener( new IValueChangeListener()
    {

      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        setEnabled( event.diff.getNewValue() != null );
      }
    } );
    
    setEnabled( triangulationBuilder.getTin() != null );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();
    final TriangulateGeometryOperation operation = new TriangulateGeometryOperation( m_triangulationBuilder, m_workspace );
    final IStatus result = ProgressUtilities.busyCursorWhile( operation );
    if( !result.isOK() )
      StatusDialog.open( shell, result, getText() );
  }

}
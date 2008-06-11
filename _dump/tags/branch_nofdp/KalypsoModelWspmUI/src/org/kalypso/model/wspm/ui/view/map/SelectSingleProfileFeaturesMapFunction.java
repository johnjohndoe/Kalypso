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
package org.kalypso.model.wspm.ui.view.map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.changers.IFeatureSelectionChanger;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.MapfunctionHelper;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.SelectFeaturesMapFunction;
import org.kalypso.ogc.gml.map.widgets.providers.IFeaturesProvider;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;

/**
 * Selects features in a map. Chooses from all themes of a certain QName (and its substitutes).
 * 
 * @author Holger Albert
 */
public class SelectSingleProfileFeaturesMapFunction extends SelectFeaturesMapFunction implements IRectangleMapFunction
{

  public SelectSingleProfileFeaturesMapFunction( final int radius, final IFeaturesProvider featuresProvider, final IFeatureSelectionChanger selectionChanger, final IFeatureSelectionManager manager )
  {
    super( radius, featuresProvider, selectionChanger, manager );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IRectangleMapFunction#execute(org.kalypso.ogc.gml.map.MapPanel,
   *      org.eclipse.swt.graphics.Rectangle)
   */
  @Override
  public void execute( final MapPanel mapPanel, final Rectangle rectangle )
  {
    final EasyFeatureWrapper[] wrappers = getFeaturesProvider().getFeatures( mapPanel );
    final EasyFeatureWrapper[] wrappersToSelect = MapfunctionHelper.findFeatureToSelect( mapPanel, rectangle, wrappers, getRadius() );
    if( wrappersToSelect != null )
    {
      if( wrappersToSelect.length > 1 )
      {
        new UIJob( "selectWspmProfile" )
        {
          @Override
          public IStatus runInUIThread( final IProgressMonitor monitor )
          {
            final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            final SelectProfileDialog dialog = new SelectProfileDialog( shell, wrappersToSelect );

            if( Window.OK == dialog.open() )
              getSelectionChanger().changeSelection( getSelectionManager(), wrappers, dialog.getSelectedCrossSection() );

            return Status.OK_STATUS;
          }

        }.schedule();
      }
      else
        getSelectionChanger().changeSelection( getSelectionManager(), wrappers, wrappersToSelect );

    }

  }

}
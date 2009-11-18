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
package org.kalypso.model.wspm.ui.action;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.IChartModel;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author kimwerner
 */
public class LengthSectionExportHandler extends AbstractHandler
{

  public static String PLOTTER_FILE_NOT_FOUND = "plotter_file_not_found";

  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IChartPart chartPart = ChartHandlerUtilities.findChartComposite( context );
    if( chartPart == null )
      return null;
    final IObservation<TupleResult> obs = getLSObservation( chartPart );
    final String plotterPath = KalypsoCorePlugin.getDefault().getPreferenceStore().getString( IWspmConstants.WSPWIN_PLOTTER_PATH );
    if( !doExport( obs, plotterPath ) )
    {
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      if( shell != null )
      {
        final FileDialog dia = new FileDialog( shell );
        dia.setFilterExtensions( new String[] { "*.exe" } ); //$NON-NLS-1$
        doExport( obs, dia.open() )

        ;

      }
    }
    return null;
  }

  private final boolean doExport( final IObservation<TupleResult> obs, final String plotterPath )
  {
    if( plotterPath == null || obs == null )
      return false;
    try
    {
      final File file = new File( System.getProperty( "java.io.tmpdir" ), "exportTmp.lng" );//$NON-NLS-1$ //$NON-NLS-2$
      final IProfilSink sink = KalypsoModelWspmCoreExtensions.createProfilSink( "lng" );
      sink.write( obs, new PrintWriter( new FileOutputStream( file ) ) );
      Runtime.getRuntime().exec( "\"" + plotterPath + "\" \"" + file.getPath() + "\"" );//$NON-NLS-1$ //$NON-NLS-2$// $NON-NLS-3$
      KalypsoCorePlugin.getDefault().getPreferenceStore().putValue( IWspmConstants.WSPWIN_PLOTTER_PATH, plotterPath );
      return true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      KalypsoCorePlugin.getDefault().getPreferenceStore().putValue( IWspmConstants.WSPWIN_PLOTTER_PATH, PLOTTER_FILE_NOT_FOUND );
    }

    return false;
  }

  private final IObservation<TupleResult> getLSObservation( final IChartPart chartPart )
  {
    IChartModel chartModel = chartPart.getChartComposite().getChartModel();
    ILayerManager layerManager = chartModel.getLayerManager();
    IChartLayer[] layers = layerManager.getLayers();
    for( IChartLayer iChartLayer : layers )
    {
      if( iChartLayer instanceof TupleResultLineLayer )
      {
        IObservation<TupleResult> obs = ((TupleResultLineLayer) iChartLayer).getObservation();
        if( obs != null )
        {
          for( final IComponent comp : obs.getResult().getComponents() )
          {
            if( comp.getId().equals( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) )
              return obs;
          }
        }
      }
    }
    return null;
  }

}

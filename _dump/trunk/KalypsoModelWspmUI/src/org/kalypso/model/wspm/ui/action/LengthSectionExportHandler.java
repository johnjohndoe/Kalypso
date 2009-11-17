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
import org.kalypso.chart.ui.editor.ui.SafeSaveDialog;
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

    final ChartComposite chart = chartPart.getChartComposite();

    String plotterPath = KalypsoCorePlugin.getDefault().getPreferenceStore().getString( IWspmConstants.WSPWIN_PLOTTER_PATH );

    if( chart != null )
    {

      if( plotterPath == "" )
      {
        final FileDialog dia = new FileDialog( (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME ) );

        dia.setFilterExtensions( new String[] { "*.exe" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        plotterPath = dia.open();
      }
      if( plotterPath != null )
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
              try
              {
                for( final IComponent comp : obs.getResult().getComponents() )
                {
                  if( comp.getId().equals( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) )
                  {
                    plotterPath = doExport( obs, plotterPath ) ;
                    KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IWspmConstants.WSPWIN_PLOTTER_PATH, plotterPath);
                    return null;
                  }
                }
              }
              catch( Exception e )
              {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
              return null;
            }
          }
        }
      }
    }
//       
// // TupleResultLineLayer x = layers[0];
// // x.getObservation();
// // // TODO: check omcponents: ists ein LS
// // // wenn ja:
// // URL locationOfLengthsectionGML = x.getLocation();
// // // -> export it
//      
//      
//      
// if( chartPart == null )
// return null;
//
// final DragZoomOutHandler plotDragZoomOutHandler = new DragZoomOutHandler( chartPart.getChartComposite() );
// chartPart.getPlotDragHandler().setActiveHandler( plotDragZoomOutHandler );
// final AxisDragZoomOutHandler axisDragZoomOutHandler = new AxisDragZoomOutHandler( chartPart.getChartComposite() );
// chartPart.getAxisDragHandler().setActiveHandler( axisDragZoomOutHandler );
// ChartHandlerUtilities.updateElements( chartPart );

    return null;
  }

  private final String doExport( final IObservation<TupleResult> obs, final String plotterPath ) throws Exception
  {
    final File file = new File( System.getProperty( "java.io.tmpdir" ), "exportTmp.lng" );//$NON-NLS-1$ //$NON-NLS-2$
    final IProfilSink sink = KalypsoModelWspmCoreExtensions.createProfilSink( "lng" );
    sink.write( obs, new PrintWriter( new FileOutputStream( file ) ) );
    try
    {
      Runtime.getRuntime().exec( "\"" + plotterPath + "\" \"" + file.getPath() + "\"" );//$NON-NLS-1$ //$NON-NLS-2$// $NON-NLS-3$
      return plotterPath;

    }
    catch( Exception e )
    {
      return "";
    }

  }
}

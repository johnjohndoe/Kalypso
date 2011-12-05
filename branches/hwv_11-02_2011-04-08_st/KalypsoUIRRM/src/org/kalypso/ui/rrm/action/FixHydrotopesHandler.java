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
package org.kalypso.ui.rrm.action;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class FixHydrotopesHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final IStructuredSelection selection = (IStructuredSelection) HandlerUtil.getCurrentSelectionChecked( event );
    final IWorkbenchPart workbenchPart = HandlerUtil.getActivePartChecked( event );
    final String commandName = HandlerUtils.getCommandName( event );

    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();

    /* Search hydrotope and catchment themes */
    final IMapModell mapModell = MapHandlerUtils.getMapModell( applicationContext );
    final IKalypsoFeatureTheme catchmentTheme = findCatchmentTheme( mapModell );
    if( catchmentTheme == null )
      return error( shell, commandName, "Unbale to find theme with catchments. Please add a catchment theme to the map." );

    final IKalypsoFeatureTheme hydrotopTheme = MapHandlerUtils.findSelectedTheme( selection, IHydrotope.QNAME );
    if( hydrotopTheme == null )
      return error( shell, commandName, "Selection does not contain a theme with hydrotopes. Please select a hydrotope theme" );

    final GMLWorkspace modellWorkspace = catchmentTheme.getFeatureList().getParentFeature().getWorkspace();
    final NaModell naModel = (NaModell) modellWorkspace.getRootFeature();

    final GMLWorkspace hydrotopeWorkspace = hydrotopTheme.getFeatureList().getParentFeature().getWorkspace();
    final NAHydrotop hydrotopes = (NAHydrotop) hydrotopeWorkspace.getRootFeature();

    /* Run operation and handle errors */
    final IProgressService progressService = (IProgressService) workbenchPart.getSite().getService( IProgressService.class );
    final IStatus result = doFixHydrotopes( hydrotopes, naModel, progressService );
    if( result.matches( IStatus.CANCEL ) )
    {
      MessageDialog.openInformation( shell, commandName, "Operation cancelled by user. Data my already have been changed. Please close map without saving hydrotope.gml." );
      return null;
    }

    if( !result.isOK() )
      new StatusDialog( shell, result, commandName ).open();

    return null;
  }

  private IStatus doFixHydrotopes( final NAHydrotop hydrotopes, final NaModell naModell, final IProgressService progressService )
  {
    final ICoreRunnableWithProgress operation = new FixHydrotopeOperation( hydrotopes, naModell );

    final String msg = "Failed to fix hydrotopes";
    return ProgressUtilities.busyCursorWhile( progressService, operation, msg );
  }

  private IKalypsoFeatureTheme findCatchmentTheme( final IMapModell mapModell )
  {
    final IKalypsoThemePredicate predicate = new IKalypsoThemePredicate()
    {
      @Override
      public boolean decide( final IKalypsoTheme theme )
      {
        if( !(theme instanceof IKalypsoFeatureTheme) )
          return false;

        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = featureTheme.getFeatureType();
        if( featureType == null )
          return false;

        return GMLSchemaUtilities.substitutes( featureType, Catchment.FEATURE_CATCHMENT );
      }
    };

    final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
    mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] catchmentThemes = visitor.getFoundThemes();
    if( catchmentThemes.length == 0 )
      return null;

    return (IKalypsoFeatureTheme) catchmentThemes[0];
  }

  private Object error( final Shell shell, final String title, final String message )
  {
    MessageDialog.openError( shell, title, message ); //$NON-NLS-1$
    return null;
  }

}

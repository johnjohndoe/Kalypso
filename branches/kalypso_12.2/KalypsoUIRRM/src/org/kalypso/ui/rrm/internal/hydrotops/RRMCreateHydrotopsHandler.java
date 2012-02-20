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
package org.kalypso.ui.rrm.internal.hydrotops;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 * @author Dejan Antanaskovic
 */
public class RRMCreateHydrotopsHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final IStructuredSelection selection = (IStructuredSelection) HandlerUtil.getCurrentSelectionChecked( event );

    final IKalypsoTheme[] themes = MapHandlerUtils.getSelectedThemes( selection );
    if( themes.length != 5 )
      return error( shell, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.0" ) ); //$NON-NLS-1$

    FeatureList flLanduse = null;
    FeatureList flPedology = null;
    FeatureList flGeology = null;
    FeatureList flCatchment = null;
    IFeatureBindingCollection<IHydrotope> flHydrotops = null;
    for( final IKalypsoTheme kalypsoTheme : themes )
    {
      final FeatureList list = ((IKalypsoFeatureTheme) kalypsoTheme).getFeatureList();
      final IRelationType featureTypeProperty = list.getPropertyType();
      final Feature parentFeature = list.getOwner();
      if( featureTypeProperty == null )
        return error( shell, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.1", kalypsoTheme.getLabel() ) ); //$NON-NLS-1$

      if( parentFeature instanceof NAHydrotop )
        flHydrotops = ((NAHydrotop) parentFeature).getHydrotopes();
      else
      {
        if( LanduseCollection.QNAME_PROP_LANDUSEMEMBER.equals( featureTypeProperty.getQName() ) )
          flLanduse = list;
        else if( SoilTypeCollection.QNAME_PROP_SOILTYPEMEMBER.equals( featureTypeProperty.getQName() ) )
          flPedology = list;
        else if( GeologyCollection.QNAME_PROP_GEOLOGYMEMBER.equals( featureTypeProperty.getQName() ) )
          flGeology = list;
        else if( NaModell.MEMBER_CATCHMENT.equals( featureTypeProperty.getQName() ) )
          flCatchment = list;

        if( list.size() == 0 )
          return error( shell, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.2", kalypsoTheme.getLabel() ) ); //$NON-NLS-1$
      }
    }
    if( flLanduse == null || flPedology == null || flGeology == null || flCatchment == null || flHydrotops == null )
      return error( shell, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.3" ) ); //$NON-NLS-1$

    final GMLWorkspace workspace = flHydrotops.getParentFeature().getWorkspace();
    final IFile outputFile = ResourceUtilities.findFileFromURL( workspace.getContext() );
    final FeatureList fflLanduse = flLanduse;
    final FeatureList fflPedology = flPedology;
    final FeatureList fflGeology = flGeology;
    final FeatureList fflCatchment = flCatchment;
    final IFeatureBindingCollection<IHydrotope> fflHydrotops = flHydrotops;

    if( !MessageDialog.openConfirm( shell, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.4" ), Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.5" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    final ICoreRunnableWithProgress operation = new CreateHydrotopesOperation( workspace, outputFile, fflCatchment, fflHydrotops, fflPedology, fflGeology, fflLanduse );
    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    final String commandName = HandlerUtils.getCommandName( event );
    StatusDialog.open( shell, status, commandName );
    // Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.6" ) );

    return null;
  }

  private Object error( final Shell shell, final String message )
  {
    MessageDialog.openError( shell, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.12" ), message ); //$NON-NLS-1$
    return null;
  }

}

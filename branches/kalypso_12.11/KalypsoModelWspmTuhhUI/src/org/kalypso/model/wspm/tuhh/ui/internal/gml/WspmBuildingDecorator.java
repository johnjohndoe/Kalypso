/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.internal.gml;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIImages;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WspmBuildingDecorator implements ILightweightLabelDecorator
{
  @Override
  public void dispose( )
  {
  }

  @Override
  public void addListener( final ILabelProviderListener listener )
  {
  }

  @Override
  public void removeListener( final ILabelProviderListener listener )
  {
  }

  @Override
  public boolean isLabelProperty( final Object element, final String property )
  {
    return true;
  }

  @Override
  public void decorate( final Object element, final IDecoration decoration )
  {
    if( element instanceof IProfileFeature )
      decorateProfile( (IProfileFeature)element, decoration );
    else if( element instanceof TuhhReachProfileSegment )
      decorateProfile( ((TuhhReachProfileSegment)element).getProfileMember(), decoration );
  }

  private void decorateProfile( final IProfileFeature profileFeature, final IDecoration decoration )
  {
    if( profileFeature == null )
      return;

    final IProfile profile = profileFeature.getProfile();
    if( profile == null )
      return;

    final IProfileObject[] buildings = profile.getProfileObjects();
    for( final IProfileObject building : buildings )
      decorateBuilding( building, decoration );
  }

  private void decorateBuilding( final IProfileObject building, final IDecoration decoration )
  {
    final String typeLabel = building.getTypeLabel();

    /* suffix */
    if( building instanceof ICulvertBuilding )
      decoration.addSuffix( String.format( Messages.getString("WspmBuildingDecorator_0"), typeLabel ) ); //$NON-NLS-1$
    else if( building instanceof GenericProfileHorizon )
    {
      // FIXME: remove this label
//      final String id = building.getType();
//      decoration.addSuffix( String.format( " ('%s')", id ) );
    }
    else
      decoration.addSuffix( String.format( " (%s)", typeLabel ) ); //$NON-NLS-1$

    /* icon */
    final ImageDescriptor buildingImage = getBuildingImage( building );
    if( buildingImage != null )
      decoration.addOverlay( buildingImage, IDecoration.REPLACE );
  }

  public static ImageDescriptor getBuildingImage( final IProfileObject building )
  {
    final PluginImageProvider imageProvider = KalypsoModelWspmTuhhUIPlugin.getImageProvider();

    if( building instanceof BuildingBruecke )
      return imageProvider.getImageDescriptor( KalypsoModelWspmTuhhUIImages.BRIDGE );

    if( building instanceof BuildingWehr )
      return imageProvider.getImageDescriptor( KalypsoModelWspmTuhhUIImages.WEIR );

    if( building instanceof BuildingKreis )
      return imageProvider.getImageDescriptor( KalypsoModelWspmTuhhUIImages.CULVERT_CIRCLE );
    if( building instanceof BuildingEi )
      return imageProvider.getImageDescriptor( KalypsoModelWspmTuhhUIImages.CULVERT_OVAL );
    if( building instanceof BuildingMaul )
      return imageProvider.getImageDescriptor( KalypsoModelWspmTuhhUIImages.CULVERT_MOUTH );
    if( building instanceof BuildingTrapez )
      return imageProvider.getImageDescriptor( KalypsoModelWspmTuhhUIImages.CULVERT_TRAPEZOID );

    return null;
  }
}
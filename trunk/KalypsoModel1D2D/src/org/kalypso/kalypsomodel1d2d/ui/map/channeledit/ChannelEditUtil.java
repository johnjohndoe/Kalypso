/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.util.JFaceProperties;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * @author Gernot Belger
 */
public final class ChannelEditUtil
{
  private ChannelEditUtil( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * converts a WSPM profile into an linestring
   *
   * @param profile
   *          Input profile to be converted.
   */
  public static LineString convertProfilesToLineStrings( final IProfileFeature profile )
  {
    // get the profile line
    final GM_Curve profCurve = profile.getLine();
    try
    {
      return (LineString) JTSAdapter.export( profCurve );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * intersects a specific linestring by a given number of intersections.<br>
   * the intersection is done by an equidistant approach. <BR>
   * this method is used for intersecting the banklines (Linestrings)
   *
   * @param linestring
   *          input linestring to be intersected
   * @param numIntersects
   *          number of intersections of the linestring
   */
  public static LineString intersectLineString( final LineString line, final int numIntersects )
  {
    if( line == null )
      return null;

    // calculate the distance between the intersection points by the distance along the profile linestring.
    final double totaldistance = line.getLength();

    // then compute the additional coodinates of the intersected profile linestring
    // by the given spinner data of the composite
    /* for now: equidistant points */
    final double dDist = totaldistance / (numIntersects - 1); // equidistant widths

    final Coordinate[] points = new Coordinate[numIntersects];

    final LengthIndexedLine lll = new LengthIndexedLine( line );

    for( int i = 0; i < points.length; i++ )
    {
      points[i] = lll.extractPoint( dDist * i );
    }

    return line.getFactory().createLineString( points );
  }

  /**
   * Create an empty profile from a template type, copying srsName and station.<br/>
   * Only components for breite, hoehe, rechtswert and hochwert are added.
   */
  public static IProfil createEmptyProfile( final IProfil templateProfile )
  {
    final String profileType = templateProfile.getType();
    final IProfil newProfil = ProfilFactory.createProfil( profileType );

    newProfil.setSrsName( templateProfile.getSrsName() );
    newProfil.setStation( templateProfile.getStation() );

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profileType );

    /* get / create components */
    final IComponent breiteComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent hwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent rwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    /* add components to new profile */
    newProfil.addPointProperty( breiteComponent );
    newProfil.addPointProperty( hoeheComponent );
    newProfil.addPointProperty( hwComponent );
    newProfil.addPointProperty( rwComponent );

    return newProfil;
  }

  public static Button createWidgetSelectionButton( final FormToolkit toolkit, final Composite parent, final CreateChannelData data, final IDataBinding binding, final SetWidgetAction action, final String enabledPropertyName )
  {
    final Button selectButton = ActionButton.createButton( toolkit, parent, action, SWT.TOGGLE );

    final IWidget delegate = action.getDelegate();

    final ISWTObservableValue targetActionSelection = SWTObservables.observeSelection( selectButton );
    final IObservableValue modelActionSelection = BeansObservables.observeValue( data, CreateChannelData.PROPERTY_DELEGATE );
    final DataBinder selectionActionBinder = new DataBinder( targetActionSelection, modelActionSelection );
    selectionActionBinder.setModelToTargetConverter( new DelagateActionSelectionConverter( delegate ) );
    binding.bindValue( selectionActionBinder );

    if( enabledPropertyName != null )
    {
      final IValueProperty enabledProperty = JFaceProperties.value( IAction.class, IAction.ENABLED, IAction.ENABLED );
      final IObservableValue targetActionEnablement = enabledProperty.observe( action );
      final IObservableValue modelProfileSelectionEnablement = BeansObservables.observeValue( data, enabledPropertyName );
      binding.bindValue( targetActionEnablement, modelProfileSelectionEnablement );
    }

    return selectButton;
  }
}
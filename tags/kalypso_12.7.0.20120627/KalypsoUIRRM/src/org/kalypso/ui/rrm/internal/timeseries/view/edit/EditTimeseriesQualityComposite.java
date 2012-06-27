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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.io.IOCase;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.validation.FileNameIsUniqueValidator;
import org.kalypso.commons.databinding.validation.MultiValidator;
import org.kalypso.commons.databinding.validation.StringFilenameValidator;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dirk Kuch
 */
public class EditTimeseriesQualityComposite extends FeatureBeanComposite<ITimeseries>
{

  public EditTimeseriesQualityComposite( final Composite parent, final IStation station, final FeatureBean<ITimeseries> featureBean, final IDataBinding binding, final boolean editable )
  {
    super( parent, featureBean, binding, editable );

    doCreateContents( station );
  }

  @Override
  protected void createContents( )
  {

  }

  protected void doCreateContents( final IStation station )
  {
    final String[] stationQualities = getQualities( station );
    final String[] textCompletionQualities = findQualities( station, stationQualities );

    final StringFilenameValidator filenameValidator = new StringFilenameValidator( IStatus.ERROR, Messages.getString( "EditTimeseriesQualityComposite_0" ) ); //$NON-NLS-1$
    final FileNameIsUniqueValidator uniqueValudator = new FileNameIsUniqueValidator( stationQualities, getQuality(), IStatus.ERROR, Messages.getString( "EditTimeseriesQualityComposite_1" ) ); //$NON-NLS-1$

    final MultiValidator validator = new MultiValidator( filenameValidator, uniqueValudator );

    createPropertyComboTextControl( ITimeseries.PROPERTY_QUALITY, textCompletionQualities, validator );

    this.layout();
  }

  private String getQuality( )
  {
    final FeatureBean<ITimeseries> bean = getBean();
    final ITimeseries timeseries = bean.getFeature();
    if( Objects.isNull( timeseries ) )
      return StringUtils.EMPTY;

    return timeseries.getQuality();
  }

  /**
   * @return possible existing qualities used for editing text completion
   */
  private String[] findQualities( final IStation current, final String[] stationQualities )
  {
    final Feature parent = current.getOwner();
    if( !(parent instanceof IStationCollection) )
      return new String[] {};

    final Set<String> found = new LinkedHashSet<>();

    final IStationCollection collection = (IStationCollection) parent;
    final IFeatureBindingCollection<IStation> stations = collection.getStations();
    for( final IStation station : stations )
    {
      if( Objects.equal( station, current ) )
        continue;

      final String[] qualities = getQualities( station );
      for( final String quality : qualities )
      {
        if( notExists( stationQualities, quality ) )
          found.add( quality );
      }
    }

    return found.toArray( new String[] {} );
  }

  private boolean notExists( final String[] existing, final String quality )
  {
    for( final String exists : existing )
    {
      if( IOCase.SYSTEM.checkEquals( exists, quality ) )
        return false;
    }

    return true;
  }

  private String[] getQualities( final IStation station )
  {
    final Set<String> qualities = new LinkedHashSet<>();

    final IFeatureBindingCollection<ITimeseries> timeserieses = station.getTimeseries();
    for( final ITimeseries ts : timeserieses )
    {
      final String quality = ts.getQuality();
      if( StringUtils.isNotEmpty( quality ) )
        qualities.add( quality );
    }

    return qualities.toArray( new String[] {} );
  }
}
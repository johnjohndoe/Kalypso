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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.validation.StringFilenameValidator;
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
  private final IValidator m_uniquenessValidator;

  public EditTimeseriesQualityComposite( final Composite parent, final IStation station, final FeatureBean<ITimeseries> featureBean, final IDataBinding binding, final boolean editable, final IValidator uniquenessValidator )
  {
    super( parent, featureBean, binding, editable );

    m_uniquenessValidator = uniquenessValidator;

    doCreateContents( station );
  }

  @Override
  protected void createContents( )
  {

  }

  protected void doCreateContents( final IStation station )
  {
    final String[] textCompletionQualities = findAllQualities( station );

    final StringFilenameValidator filenameValidator = new StringFilenameValidator( IStatus.ERROR, Messages.getString( "EditTimeseriesQualityComposite_0" ) ); //$NON-NLS-1$

    createPropertyComboTextControl( ITimeseries.PROPERTY_QUALITY, textCompletionQualities, filenameValidator, m_uniquenessValidator );
  }

  /**
   * @return possible existing qualities used for editing text completion
   */
  private static String[] findAllQualities( final IStation current )
  {
    final Feature parent = current.getOwner();
    if( !(parent instanceof IStationCollection) )
      return new String[] {};

    final Comparator<String> stationComparator = new Comparator<String>()
    {
      @Override
      public int compare( final String o1, final String o2 )
      {
        return o1.compareToIgnoreCase( o2 );
      }
    };

    final Set<String> found = new TreeSet<>( stationComparator );

    final IStationCollection collection = (IStationCollection) parent;
    final IFeatureBindingCollection<IStation> stations = collection.getStations();
    for( final IStation station : stations )
    {
      final String[] qualities = station.getQualities( null );
      for( final String quality : qualities )
      {
        found.add( quality );
      }
    }

    return found.toArray( new String[] {} );
  }
}

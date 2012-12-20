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
package org.kalypso.ui.rrm.internal.cm;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.rcm.binding.IThiessenStationCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.CatchmentBean;
import org.kalypso.ui.rrm.internal.cm.view.FactorizedTimeseriesBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

import com.google.common.collect.TreeMultimap;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * This class contains functions which are used by the linear sum generator, Thiessen Method and Inverse Distance
 * Weighting Method.
 *
 * @author Holger Albert
 */
public class LinearSumHelper
{
  /**
   * The constructor.
   */
  private LinearSumHelper( )
  {
  }

  /**
   * This function creates the linear sum bean for the given parameter type from the current scenario.
   *
   * @param parameterType
   *          The parameter type.
   * @return The linear sum bean.
   */
  public static LinearSumBean createFromCurrentScenario( final String parameterType )
  {
    try
    {
      /* Get the NA model. */
      final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final NaModell model = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_MODEL );

      /* Create the linear sum bean. */
      final LinearSumBean bean = LinearSumBean.createFromModel( model );
      bean.setProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE, parameterType );

      return bean;
    }
    catch( final CoreException e )
    {
      // If this happens, it's an internal bug!
      e.printStackTrace();
      return null;
    }
  }

  /**
   * This function loads the stations gml.
   *
   * @return The collection of stations. Each station corresponds to a timeseries. So this is better spoken a list of
   *         timeseries (which may have the same station).
   */
  public static IThiessenStationCollection loadStationsGml( ) throws CoreException
  {
    try
    {
      /* Timeseries file. */
      final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();
      final RrmScenario rrmScenario = new RrmScenario( scenarioFolder );
      final IFile stationFile = rrmScenario.getThiessenTempFile();

      /* Load the file and transform to the kalypso coordinate system. */
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( stationFile );
      workspace.accept( new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() ), FeatureVisitor.DEPTH_INFINITE );

      return (IThiessenStationCollection) workspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "LinearSumHelper_0" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  /**
   * This function deletes the generated stations gml. It will not throw an exception on failure. The stations gml is
   * purly temporary and will be overwritten the next time the wizard opens if it was not deleted.
   */
  public static void deleteStationsGmlQuietly( )
  {
    try
    {
      final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();

      final RrmScenario rrmScenario = new RrmScenario( scenarioFolder );
      final IFile stationFile = rrmScenario.getThiessenTempFile();

      stationFile.delete( true, new NullProgressMonitor() );
    }
    catch( final Exception ex )
    {
      // Ignore
      ex.printStackTrace();
    }
  }

  /**
   * This function applys the weights to the catchment.
   *
   * @param catchment
   *          The catchment.
   * @param timeseries
   *          The timeseries (in the same order as the weights).
   * @param weights
   *          The weights.
   */
  public static void apply( final CatchmentBean catchment, final String[] timeseries, final double[] weights )
  {
    /* Adjust the weights. */
    final FactorElement[] factors = adjustWeights( weights );

    /* Apply the factors to the bean. */
    for( int i = 0; i < factors.length; i++ )
    {
      /* Get the factor. */
      final int factor = factors[i].getFactor();

      /* Determine the href. */
      final String href = timeseries[i];

      /* Set the factor. */
      final FactorizedTimeseriesBean factorizedTimeseries = catchment.getTimeseries( href );
      if( factorizedTimeseries != null )
        factorizedTimeseries.setFactor( factor );
    }
  }

  /**
   * This function adjusts the weights.
   * <ul>
   * <li>Converts the weights to factors (in percent).</li>
   * <li>Makes sure the factors does not exceed the 100%.</li>
   * </ul>
   *
   * @param weights
   *          The weights. The sum of weights must result in 1.0.
   * @return The factors (in percent).
   */
  private static FactorElement[] adjustWeights( final double[] weights )
  {
    /* Memory for the factors. */
    final int[] factors = new int[weights.length];
    int sumFactors = 0;

    /* Convert the weights to factors. */
    for( int i = 0; i < weights.length; i++ )
    {
      /* Get the weight. */
      final double weight = weights[i];

      /* Converte the weight to a factor. */
      factors[i] = (int) Math.round( weight * 100.0 );

      /* Add to the sum of factors. */
      sumFactors = sumFactors + factors[i];
    }

    return modifyFactors( factors, sumFactors );
  }

  private static FactorElement[] modifyFactors( final int[] factors, final int sumFactors )
  {
    /* Keep the order of the factors. */
    final FactorElement[] elements = new FactorElement[factors.length];

    /* HINT: The index makes the object unique, because factors may be equal. */
    /* HINT: The index will be used in the compare method in the factor element. */
    /* HINT: This method is used by the tree multimap to determine equivalence. */
    for( int i = 0; i < factors.length; i++ )
      elements[i] = new FactorElement( factors[i], i );

    /* Calculate the difference to 100. */
    int difference = sumFactors - 100;
    if( difference == 0 )
      return elements;

    /* The sign of the difference. */
    int signum = 1;
    if( difference < 0 )
      signum = -1;

    /* This sorted map allows a key with multiple values. */
    final TreeMultimap<Double, FactorElement> map = TreeMultimap.create();

    /* Fill the sorted map. */
    for( final FactorElement element : elements )
    {
      /* HINT: If the sum is greater than 100 we want to substract values from the smallest factors first. */
      /* HINT: Multiplying the factors (used as keys) with the signum */
      /* HINT: results in a normal ordered list with positive keys. */
      /* HINT: If the sum is smaller than 100 we want to add values to the greatest factors first. */
      /* HINT: Multiplying the factors (used as keys) with the signum */
      /* HINT: results in a reversed list with negative keys. */
      final int factor = signum * element.getFactor();

      /* Put the key and the value. */
      map.put( new Double( factor ), element );
    }

    /* HINT: Now the keys of the map will be sorted by their natural order. */
    /* HINT: Now the values of the map will be sorted by the natural order of the index. */
    /* HINT: Only relevant if a key has more than one value. */

    /* Calculate the modification value. */
    final int mod = -1 * signum;

    /* Finally change the factors... */
    final Double[] keys = map.keySet().toArray( new Double[] {} );
    for( final Double key : keys )
    {
      /* HINT: Here the elements will be sorted by the natural order of the index. */
      final SortedSet<FactorElement> sortedSet = map.get( key );
      for( final FactorElement factorElement : sortedSet )
      {
        /* Ignore 0.0 factors. */
        final int factor = factorElement.getFactor();
        if( factor == 0.0 )
          continue;

        /* Adjust the factor. */
        factorElement.setFactor( factor + mod );

        /* Adjust the difference. */
        difference = difference + mod;

        /* Nothing more to do. */
        if( difference == 0 )
          return elements;
      }
    }

    return elements;
  }

  public static ITimeseries[] collectTimeseries( final LinearSumBean bean )
  {
    final List<ITimeseries> timeseries = new ArrayList<>();

    final CatchmentBean[] catchments = bean.getCatchments();
    for( final CatchmentBean catchment : catchments )
    {
      final FactorizedTimeseriesBean[] allTimeseries = catchment.getTimeseries();
      for( final FactorizedTimeseriesBean oneTimeseries : allTimeseries )
      {
        final int factor = oneTimeseries.getFactor();
        if( factor <= 0 )
          continue;

        final ITimeseries feature = oneTimeseries.getFeature();
        timeseries.add( feature );
      }
    }

    return timeseries.toArray( new ITimeseries[] {} );
  }

  public static DateRange createDateRange( final LinearSumBean bean )
  {
    final Object validFromCalendar = bean.getProperty( ILinearSumGenerator.PROPERTY_VALID_FROM );
    final Object validToCalendar = bean.getProperty( ILinearSumGenerator.PROPERTY_VALID_TO );
    if( validFromCalendar == null || validToCalendar == null )
      return null;

    final Date validFromDate = DateUtilities.toDate( validFromCalendar );
    final Date validToDate = DateUtilities.toDate( validToCalendar );

    return new DateRange( validFromDate, validToDate );
  }
}
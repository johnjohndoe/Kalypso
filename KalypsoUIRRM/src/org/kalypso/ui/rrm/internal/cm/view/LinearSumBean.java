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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class LinearSumBean extends FeatureBean<ILinearSumGenerator>
{
  private CatchmentBean[] m_catchments;

  /**
   * Constructs a complete empty linear sum bean.
   */
  public LinearSumBean( )
  {
    super( ILinearSumGenerator.FEATURE_LINEAR_SUM_GENERATOR );

    m_catchments = new CatchmentBean[] {};

    initValidities();
  }

  /**
   * Constructs a linear sum bean initialized with the values from the linear sum generator.
   *
   * @param generator
   *          The linear sum generator.
   */
  public LinearSumBean( final ILinearSumGenerator generator )
  {
    super( generator );

    m_catchments = initCatchments();

    initValidities();
  }

  public CatchmentBean[] getCatchments( )
  {
    return m_catchments;
  }

  public void setCatchments( final CatchmentBean[] catchments )
  {
    m_catchments = catchments;
  }

  /**
   * This function applies the changes of this linear sum generator. It will create or update the linear sum generator
   * feature. If the linear sum generator feature is existing, its (probably) existing catchment features will be
   * deleted. So always new ones will be generated.
   *
   * @param workspace
   *          The workspace.
   * @param parameterType
   *          The selected parameter type.
   * @return The new linear sum generator.
   */
  public ILinearSumGenerator apply( final CommandableWorkspace workspace, final String parameterType ) throws Exception
  {
    /* Get the feature. */
    final ILinearSumGenerator feature = getFeature();
    if( feature == null )
    {
      /* The linear sum generator feature does not exist. */
      final Map<QName, Object> properties = new HashMap<>( getProperties() );
      final ICatchmentModel collection = (ICatchmentModel) workspace.getRootFeature();
      final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchmentModel.MEMBER_CATCHMENT_GENERATOR );
      final QName type = getFeatureType().getQName();

      /* Create the add feature command. */
      final AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );

      /* Post the command. */
      workspace.postCommand( command );

      /* Apply also all contained catchments. */
      for( final CatchmentBean catchment : m_catchments )
        catchment.apply( workspace, command.getNewFeature(), parameterType );

      /* Get the new feature. */
      final ILinearSumGenerator newFeature = (ILinearSumGenerator) command.getNewFeature();

      /* Adjust the validities. */
      newFeature.adjustValidities();

      /* Set the last modified timestamp. */
      newFeature.setLastModified( new Date() );

      return newFeature;
    }

    /* Apply the changes. */
    final ICommand command = applyChanges();

    /* Post the command. */
    workspace.postCommand( command );

    /* The delete commands. */
    final CompositeCommand deleteCommands = new CompositeCommand( Messages.getString( "LinearSumBean_0" ) ); //$NON-NLS-1$

    /* Get all catchments. */
    final List<ICatchment> catchments = feature.getCatchments();
    for( final ICatchment catchment : catchments )
      deleteCommands.addCommand( new DeleteFeatureCommand( catchment ) );

    /* Post the command. */
    workspace.postCommand( deleteCommands );

    /* Apply also all contained catchments. */
    for( final CatchmentBean catchment : m_catchments )
      catchment.apply( workspace, feature, parameterType );

    /* Adjust the validities. */
    feature.adjustValidities();

    /* Set the last modified timestamp. */
    feature.setLastModified( new Date() );

    return feature;
  }

  public boolean hasDescription( )
  {
    final Object property = getProperty( ILinearSumGenerator.QN_DESCRIPTION );
    if( property == null )
      return false;

    return true;
  }

  private CatchmentBean[] initCatchments( )
  {
    final List<CatchmentBean> results = new ArrayList<>();
    final ILinearSumGenerator generator = getFeature();
    final List<ICatchment> catchments = generator.getCatchments();
    for( final ICatchment catchment : catchments )
      results.add( new CatchmentBean( catchment ) );

    return results.toArray( new CatchmentBean[] {} );
  }

  private void initValidities( )
  {
    final Date currentDate = new Date();

    final Object validFrom = getProperty( ILinearSumGenerator.PROPERTY_VALID_FROM );
    if( validFrom == null )
      setProperty( ILinearSumGenerator.PROPERTY_VALID_FROM, DateUtilities.toXMLGregorianCalendar( currentDate ) );

    final Object validTo = getProperty( ILinearSumGenerator.PROPERTY_VALID_TO );
    if( validTo == null )
      setProperty( ILinearSumGenerator.PROPERTY_VALID_TO, DateUtilities.toXMLGregorianCalendar( currentDate ) );
  }

  /**
   * Creates a empty linear sum bean with all catchments from the model.gml.
   *
   * @param model
   *          The na model.
   * @return The linear sum bean.
   */
  public static LinearSumBean createFromModel( final NaModell model )
  {
    final LinearSumBean bean = new LinearSumBean();
    bean.setProperty( ILinearSumGenerator.PROPERTY_AREA_NAME, "gml:name" ); //$NON-NLS-1$
    bean.setProperty( ILinearSumGenerator.PROPERTY_AREA_DESCRIPTION, "gml:description" ); //$NON-NLS-1$
    bean.setProperty( ILinearSumGenerator.PROPERTY_AREA, Catchment.PROP_GEOM.getLocalPart() );

    final CatchmentBean[] beans = getCatchmentsFromModel( model );
    bean.setCatchments( beans );

    return bean;
  }

  /**
   * Creates a linear sum bean representing the linear sum generator with all catchments from the model.gml. It keeps
   * the factors, where possible.
   *
   * @param model
   *          The na model.
   * @param generator
   *          The linear sum generator.
   * @return The linear sum bean.
   */
  public static LinearSumBean reinitFromModel( final NaModell model, final ILinearSumGenerator generator )
  {
    /* Create a new linear sum bean. */
    final LinearSumBean bean = new LinearSumBean();
    bean.setFeature( generator );

    /* Get the catchments from the model. */
    final CatchmentBean[] modelCatchments = getCatchmentsFromModel( model );

    /* Get the catchments from the generator. */
    final Map<String, ICatchment> generatorCatchments = getCatchmentsFromGenerator( generator );

    /* Add the factors of the generator, where possible. */
    updateCatchments( modelCatchments, generatorCatchments );

    /* Set the catchments of the model. */
    bean.setCatchments( modelCatchments );

    return bean;
  }

  private static CatchmentBean[] getCatchmentsFromModel( final NaModell model )
  {
    final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
    final Collection<CatchmentBean> catchmentBeans = new ArrayList<>( catchments.size() );

    for( final Catchment catchment : catchments )
    {
      final String catchmentId = catchment.getId();
      final String catchmentName = catchment.getName();
      final String catchmentDescription = catchment.getDescription();
      final GM_Polygon catchmentArea = catchment.getGeometry();

      final CatchmentBean catchmentBean = new CatchmentBean();
      catchmentBean.setCatchmentRef( catchmentId );
      catchmentBean.setCatchmentName( catchmentName );
      catchmentBean.setCatchmentDescription( catchmentDescription );
      catchmentBean.setCatchmentArea( catchmentArea );

      catchmentBeans.add( catchmentBean );
    }

    return catchmentBeans.toArray( new CatchmentBean[catchmentBeans.size()] );
  }

  private static Map<String, ICatchment> getCatchmentsFromGenerator( final ILinearSumGenerator generator )
  {
    final Map<String, ICatchment> results = new HashMap<>();

    final IFeatureBindingCollection<ICatchment> catchments = generator.getCatchments();
    for( final ICatchment catchment : catchments )
    {
      final IXLinkedFeature generatorLink = (IXLinkedFeature) catchment.getAreaLink();
      final String generatorId = generatorLink.getFeatureId();
      results.put( generatorId, catchment );
    }

    return results;
  }

  private static void updateCatchments( final CatchmentBean[] modelCatchments, final Map<String, ICatchment> generatorCatchments )
  {
    for( final CatchmentBean modelCatchment : modelCatchments )
    {
      /* Get the catchment of the generator for the id of the catchment of the model. */
      final ICatchment generatorCatchment = generatorCatchments.remove( modelCatchment.getCatchmentRef() );

      /* If there is none, this is a new catchment in the model. */
      /* No factors can be set. */
      if( generatorCatchment == null )
        continue;

      /* Get the generator timeseries. */
      final IFeatureBindingCollection<IFactorizedTimeseries> generatorTimeseries = generatorCatchment.getFactorizedTimeseries();

      /* Initialize the model catchment with the factors of the generator timeseries. */
      modelCatchment.initializeFactors( generatorTimeseries );
    }

    /* HINT: If there generator catchments left, they will be missing now in the model catchments. */
    /* HINT: Hence, the factors are lost. */
  }

  public void resetTimeseries( )
  {
    /* Clear all old weights. */
    for( final CatchmentBean catchment : m_catchments )
      catchment.clearAllWeights();
  }
}
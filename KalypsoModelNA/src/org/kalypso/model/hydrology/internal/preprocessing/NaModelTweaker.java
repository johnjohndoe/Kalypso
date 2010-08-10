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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.util.Iterator;
import java.util.List;

import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Before any ascii files are written, the modell.gml (calcCase.gml) gets tweaked by this class.<br/>
 * 
 * @author Gernot Belger
 */
public class NaModelTweaker
{
  private static String[] CATCHMENT_FACTOR_PARAMETER_TARGET = { "retob", "retint", "aigw" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private final static String[][] CATCHMENT_FACTORS_PARAMETER = { new String[] { "retob", "faktorRetobRetint" }, new String[] { "retint", "faktorRetobRetint" }, new String[] { "aigw", "faktorAigw" } }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

  private final GMLWorkspace m_modelWorkspace;

  private final NaNodeResultProvider m_nodeResultProvider;

  public NaModelTweaker( final GMLWorkspace modelWorkspace, final NaNodeResultProvider nodeResultProvider )
  {
    m_modelWorkspace = modelWorkspace;
    m_nodeResultProvider = nodeResultProvider;
  }

  /**
   * update workspace and do some tricks in order to fix some things the fortran-kernel can not handle for now
   * 
   * @param workspace
   * @throws Exception
   */
  public void tweakModel( ) throws Exception
  {
    updateGWNet();
    updateNode2NodeNet();
    updateZuflussNet();
    updateResultAsZuflussNet();
    updateFactorParameter();
  }

  /**
   * before: <code>
   * 
   *     o(existing)
   * 
   * </code> after: <code>
   * 
   *  |new Channel3|
   *     |
   *     V
   *     o(new Node2)  (return value)
   *     |
   *     V
   *  |new Channel1|
   *     |
   *     V
   *     o(existing)
   * 
   * </code>
   */
  private Feature buildVChannelNet( final Feature existingNode ) throws Exception
  {
    final IGMLSchema gmlSchema = m_modelWorkspace.getGMLSchema();

    final IFeatureType nodeColFT = gmlSchema.getFeatureType( NaModelConstants.NODE_COLLECTION_FT );
    final IFeatureType nodeFT = gmlSchema.getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IRelationType nodeMemberRT = (IRelationType) nodeColFT.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    final IFeatureType vChannelFT = gmlSchema.getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );

    final IFeatureType channelColFT = gmlSchema.getFeatureType( NaModelConstants.NA_CHANNEL_COLLECTION_FT );
    final IRelationType channelMemberRT = (IRelationType) channelColFT.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    final Feature channelColFE = m_modelWorkspace.getFeatures( channelColFT )[0];
    final Feature nodeColFE = m_modelWorkspace.getFeatures( gmlSchema.getFeatureType( NaModelConstants.NODE_COLLECTION_FT ) )[0];

    // add to collections:
    final Feature newChannelFE1 = m_modelWorkspace.createFeature( channelColFE, channelMemberRT, vChannelFT );
    m_modelWorkspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE1 );
    final Feature newChannelFE3 = m_modelWorkspace.createFeature( channelColFE, channelMemberRT, vChannelFT );
    m_modelWorkspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE3 );
    final Feature newNodeFE2 = m_modelWorkspace.createFeature( nodeColFE, nodeMemberRT, nodeFT );
    m_modelWorkspace.addFeatureAsComposition( nodeColFE, nodeMemberRT, 0, newNodeFE2 );
    final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );

    // 3 -> 2
    m_modelWorkspace.setFeatureAsAggregation( newChannelFE3, downStreamNodeMemberRT, newNodeFE2.getId(), true );
    // 2 -> 1
    final IRelationType downStreamChannelMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
    m_modelWorkspace.setFeatureAsAggregation( newNodeFE2, downStreamChannelMemberRT, newChannelFE1.getId(), true );
    // 1 -> existing

    // final IRelationType downStreamNodeMemberRT1 = (IRelationType) vChannelFT.getProperty( "downStreamNodeMember" );
    m_modelWorkspace.setFeatureAsAggregation( newChannelFE1, downStreamNodeMemberRT, existingNode.getId(), true );
    return newNodeFE2;
  }

  /**
   * Updates workspace, so that interflow and channelflow dependencies gets optimized <br>
   * Groundwater flow can now run in opposite direction to channel flow.<br>
   * before: <code>
   * 
   *     -C-o (existing channel c with catchment T and downstream node o)
   *      ^
   *      T
   * 
   * </code> after: <code>
   * 
   *     -C-o 
   *        |
   *        V<T  (new virtual channel with existing catchment T, downstream node o and no upstream node)
   * 
   * </code>
   * 
   * @param workspace
   */
  private void updateGWNet( )
  {
    final NaModell naModel = (NaModell) m_modelWorkspace.getRootFeature();

    final IGMLSchema gmlSchema = m_modelWorkspace.getGMLSchema();

    final IFeatureType vChannelFT = gmlSchema.getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );

    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      final IRelationType catchmentRT = (IRelationType) catchment.getFeatureType().getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
      final Feature channelFE = m_modelWorkspace.resolveLink( catchment, catchmentRT );
      if( channelFE == null )
        continue;

      final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
      final Feature nodeFE = m_modelWorkspace.resolveLink( channelFE, downStreamNodeMemberRT );

      final Feature newChannelFE = m_modelWorkspace.createFeature( catchment, catchmentRT, vChannelFT );
      try
      {
        // set new relation: catchment -> new V-channel
        m_modelWorkspace.setFeatureAsComposition( catchment, catchmentRT, newChannelFE, true );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      // set new relation: new V-channel -> downstream node
      try
      {
        final IRelationType downStreamNodeMemberRT2 = (IRelationType) newChannelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
        m_modelWorkspace.addFeatureAsAggregation( newChannelFE, downStreamNodeMemberRT2, 1, nodeFE.getId() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * before: <br>
   * <code>
   * 
   * Node1 O <---  O Node2
   * 
   * </code> after: <br>
   * <code>
   * 
   * Node1 O <--- newVChannel <-- newNode O <-- newVChannel
   *                                      A
   *                                      |
   *                                      O-- Node2
   * 
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateNode2NodeNet( ) throws Exception
  {
    final IGMLSchema gmlSchema = m_modelWorkspace.getGMLSchema();

    final IFeatureType kontEntnahmeFT = gmlSchema.getFeatureType( NaModelConstants.NODE_VERZW_ENTNAHME );
    final IFeatureType ueberlaufFT = gmlSchema.getFeatureType( NaModelConstants.NODE_VERZW_UEBERLAUF );
    final IFeatureType verzweigungFT = gmlSchema.getFeatureType( NaModelConstants.NODE_VERZW_VERZWEIGUNG );
    final IFeatureType nodeFT = gmlSchema.getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] features = m_modelWorkspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
    for( final Feature nodeFE : features )
    {
      final Feature branchingFE = m_modelWorkspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null )
      {
        final IFeatureType branchFT = branchingFE.getFeatureType();
        final IRelationType branchingNodeMemberRT = (IRelationType) branchFT.getProperty( NaModelConstants.NODE_BRANCHING_NODE_MEMBER_PROP );
        if( branchFT == kontEntnahmeFT || branchFT == ueberlaufFT || branchFT == verzweigungFT )
        {
          final Feature targetNodeFE = m_modelWorkspace.resolveLink( branchingFE, branchingNodeMemberRT );
          if( targetNodeFE == null )
          {
            final String relationLabel = branchingNodeMemberRT.getAnnotation().getLabel();
            final String branchingFElabel = FeatureHelper.getAnnotationValue( branchingFE, IAnnotation.ANNO_LABEL );
            final String message = String.format( "'%s' not set for '%s' in Node '%s'", relationLabel, branchingFElabel, nodeFE.getName() );
            throw new SimulationException( message );
          }

          final Feature newNodeFE = buildVChannelNet( targetNodeFE );
          m_modelWorkspace.setFeatureAsComposition( branchingFE, branchingNodeMemberRT, newNodeFE, true );
        }
      }
    }
  }

  /**
   * put one more VChannel to each Q-source, so that this discharge will appear in the result of the connected node <br>
   * zml inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- input.zml <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- input.zml <br>
   * </code> constant inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- Q(constant) <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- Q(constant)<br>
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateZuflussNet( ) throws Exception
  {
    final IGMLSchema gmlSchema = m_modelWorkspace.getGMLSchema();

    final IFeatureType kontZuflussFT = gmlSchema.getFeatureType( NaModelConstants.NODE_VERZW_ZUFLUSS );
    final IFeatureType nodeFT = gmlSchema.getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] features = m_modelWorkspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
    for( final Feature nodeFE : features )
    {
      final Object zuflussValue = nodeFE.getProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP );
      if( zuflussValue != null )
      {
        // update zufluss
        final Feature newNode = buildVChannelNet( nodeFE );
        // nove zufluss-property to new node
        // nodeFE.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", null ) );
        // newNode.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", zuflussValue ) );
        nodeFE.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, null );
        newNode.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, zuflussValue );
        newNode.setProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP, nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) );
      }
      final Feature branchingFE = m_modelWorkspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null && branchingFE.getFeatureType() == kontZuflussFT )
      {
        // update zufluss
        final Feature newNode = buildVChannelNet( nodeFE );
        // nove constant-inflow to new node
        m_modelWorkspace.setFeatureAsComposition( nodeFE, branchingMemberRT, null, true );
        m_modelWorkspace.setFeatureAsComposition( newNode, branchingMemberRT, branchingFE, true );
      }
    }
  }

  /**
   * if results exists (from a former simulation) for a node, use this results as input, later the upstream nodes will
   * be ignored for calculation
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateResultAsZuflussNet( ) throws Exception
  {
    final IGMLSchema gmlSchema = m_modelWorkspace.getGMLSchema();

    final IFeatureType nodeFT = gmlSchema.getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IFeatureType abstractChannelFT = gmlSchema.getFeatureType( NaModelConstants.CHANNEL_ABSTRACT_FT );
    final Feature[] features = m_modelWorkspace.getFeatures( nodeFT );
    for( final Feature nodeFE : features )
    {
      if( m_nodeResultProvider.checkResultExists( nodeFE ) )
      {
        final Object resultValue = nodeFE.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );
        // disconnect everything upstream (channel -> node)
        final IRelationType downStreamNodeMemberRT = (IRelationType) abstractChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
        final Feature[] channelFEs = m_modelWorkspace.resolveWhoLinksTo( nodeFE, abstractChannelFT, downStreamNodeMemberRT );
        for( final Feature element : channelFEs )
        {
          final Feature newEndNodeFE = m_modelWorkspace.createFeature( element, downStreamNodeMemberRT, nodeFT );
          m_modelWorkspace.setFeatureAsComposition( element, downStreamNodeMemberRT, newEndNodeFE, true );
        }
        // add as zufluss
        final Feature newNodeFE = buildVChannelNet( nodeFE );
        // final FeatureProperty zuflussProp = FeatureFactory.createFeatureProperty( "zuflussZR", resultValue );
        newNodeFE.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, resultValue );
        newNodeFE.setProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP, nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) );
      }
    }
  }

  /**
   * Updates model with factor values from control<br>
   * some parameter have factors that must be processed before generating asciifiles, as these factors do not occur in
   * ascci-format
   * 
   * @param modellWorkspace
   */
  private void updateFactorParameter( )
  {
    final IGMLSchema gmlSchema = m_modelWorkspace.getGMLSchema();
    final NaModell naModel = (NaModell) m_modelWorkspace.getRootFeature();

    // Catchments
    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
    for( final Catchment catchment : catchments )
      updateCatchment( catchment );

    // KMChannels
    final Feature[] kmChanneFEs = m_modelWorkspace.getFeatures( gmlSchema.getFeatureType( NaModelConstants.KM_CHANNEL_ELEMENT_FT ) );
    for( final Feature kmChanneFE : kmChanneFEs )
      updateKMChannel( kmChanneFE );
  }

  private void updateCatchment( final Catchment catchmentFE )
  {
    for( int _p = 0; _p < CATCHMENT_FACTOR_PARAMETER_TARGET.length; _p++ ) // iterate parameters
    {
      final String[] factors = CATCHMENT_FACTORS_PARAMETER[_p];
      double value = 1.0; // initial value
      for( final String element : factors )
        // iterate factors
        value *= FeatureHelper.getAsDouble( catchmentFE, element, 1.0 );
      // set parameter
      final String targetPropName = CATCHMENT_FACTOR_PARAMETER_TARGET[_p];
      // FeatureProperty valueProp = FeatureFactory.createFeatureProperty( targetPropName, new Double( value ) );
      catchmentFE.setProperty( targetPropName, new Double( value ) );
    }
  }

  private void updateKMChannel( final Feature kmChanneFE )
  {
    final double rkfFactor = FeatureHelper.getAsDouble( kmChanneFE, NaModelConstants.KM_CHANNEL_FAKTOR_RKF_PROP, 1.0 );
    final double rnfFactor = FeatureHelper.getAsDouble( kmChanneFE, NaModelConstants.KM_CHANNEL_FAKTOR_RNF_PROP, 1.0 );
    final List< ? > kmParameter = (List< ? >) kmChanneFE.getProperty( NaModelConstants.KM_CHANNEL_PARAMETER_MEMBER );
    final Iterator< ? > iterator = kmParameter.iterator();
    while( iterator.hasNext() )
    {
      final Feature kmParameterFE = (Feature) iterator.next();
      // rnf
      final double _rnf = rnfFactor * FeatureHelper.getAsDouble( kmParameterFE, NaModelConstants.KM_CHANNEL_RNF_PROP, 1.0 );
      // FeatureProperty rnfProp = FeatureFactory.createFeatureProperty( "rnf", new Double( _rnf ) );
      kmParameterFE.setProperty( NaModelConstants.KM_CHANNEL_RNF_PROP, new Double( _rnf ) );
      // rkf
      final double _rkf = rkfFactor * FeatureHelper.getAsDouble( kmParameterFE, NaModelConstants.KM_CHANNEL_RKF_PROP, 1.0 );
      // FeatureProperty rkfProp = FeatureFactory.createFeatureProperty( "rkf", new Double( _rkf ) );
      kmParameterFE.setProperty( NaModelConstants.KM_CHANNEL_RKF_PROP, new Double( _rkf ) );
    }
  }

}

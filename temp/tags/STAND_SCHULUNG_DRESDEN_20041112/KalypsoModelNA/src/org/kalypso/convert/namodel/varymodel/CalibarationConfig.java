package org.kalypso.convert.namodel.varymodel;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class CalibarationConfig
{

  private final List m_contexts;

  public CalibarationConfig()
  {
    m_contexts = new ArrayList();
  }

  public void addFromNAControl( Feature rootFeature )
  {
    // Catchments
    final String queryBaseCatchment = FeatureHelper.getAsString( rootFeature, "Catchments" );
    String[] propNamesI = new String[]
    {
        "CatchmentsBianf",
        "CatchmentsFaktorRetobTetint",
        "CatchmentsFaktn",
        "CatchmentsFaktorAigw" };

    String[][] queryCatchments = new String[][]
    {
        new String[]
//        { queryBaseCatchment + "//banf" },
        { queryBaseCatchment + "/faktorBianf" },
        new String[]
        { queryBaseCatchment + "/faktorRetobRetint" },
        new String[]
        { queryBaseCatchment + "/faktn" },
        new String[]
        { queryBaseCatchment + "/faktorAigw" } };

    generateAndAddContexts( rootFeature, queryCatchments, propNamesI );
    // KMChannels
    final String queryBaseKMChannel = FeatureHelper.getAsString( rootFeature, "KMChannels" );
    final String[] propNamesII = new String[]
    {
        "KMChannelsFaktorRkf",
        "KMChannelsFaktorRnf" };

    final String[][] queryKMChannels = new String[][]
    {
        new String[]
        { queryBaseKMChannel + "/faktorRkf" },
        new String[]
        { queryBaseKMChannel + "/faktorRnf" } };
    generateAndAddContexts( rootFeature, queryKMChannels, propNamesII );
  }

  private void generateAndAddContexts( Feature rootFeature, String[][] queryStrings,
      String[] propNames )
  {
    final int n = propNames.length;
    for( int i = 0; i < n; i++ )
    {
      final String value = FeatureHelper.getAsString( rootFeature, propNames[i] );
      if( value == null || value.length() == 0 )
        return;
      final double initialValue = Double.parseDouble( value );
      final String[] xPaths = queryStrings[i];
      addContext( new CalContext( initialValue, 1, 0, 2, CalContext.MODE_DIRECT, xPaths ) );
    }
  }

  //  private void addFromConfFile( URL callibrationConfigurationURL )
  //  {
  //    Document doc;
  //    try
  //    {
  //      doc = XMLServiceTools.getXML( callibrationConfigurationURL.openStream() );
  //      CalContext[] calContexts = read( doc );
  //    }
  //    catch( Exception e )
  //    {
  //      e.printStackTrace();
  //    }
  //  }

  public CalContext[] getCalContexts()
  {
    return (CalContext[])m_contexts.toArray( new CalContext[m_contexts.size()] );
  }

  public void addContext( CalContext context )
  {
    m_contexts.add( context );
  }

  //  private CalContext[] read( Document doc ) throws TransformerException
  //  {
  //    //Gauging station
  //    // String queryfile = "/autoCalibration/pegel/file";
  //    // Node file = xmlServiceTools.getXPath_singleNode( queryfile, doc );
  //    // Node fValue = file.getFirstChild();
  //    // String fileValue = fValue.getNodeValue();
  //    // gaugeFile = fileValue;
  //    // if( fileValue.equals( "syntetic" ) )
  //    // {
  //    // syntetic = true;
  //    // }
  //    // else
  //    // {
  //    // syntetic = false;
  //    // }
  //    // System.out.println( "Syntetic: " + syntetic );
  //
  //    //Objectivefunction values
  //    // String queryTrafoConstants =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/transformationConstants";
  //    // Node tConstants = XMLServiceTools.getXPath_singleNode(
  //    // queryTrafoConstants, doc );
  //    // Node trafoValue = tConstants.getFirstChild();
  //    // String trafoConstantsValue = trafoValue.getNodeValue();
  //    // if( trafoConstantsValue.equals( "equal" ) )
  //    // {
  //    // equalWeights = true;
  //    // }
  //    // if( trafoConstantsValue.equals( "userdefined" ) )
  //    // {
  //    // equalWeights = false;
  //    // }
  //
  //    //Volume Error
  //    // String queryVolumeError_mode =
  //    // "/autoCalibration/optParameter/objectiveFunction/volumeError/@mode";
  //    // Node mode = xmlServiceTools.getXPath_singleNode( queryVolumeError_mode,
  //    // doc );
  //    // Node mValue = mode.getFirstChild();
  //    // String modeValue = mValue.getNodeValue();
  //    // objectiveFunctions[0] = ( new Boolean( modeValue ) ).booleanValue();
  //    // if( !equalWeights && objectiveFunctions[0] )
  //    // {
  //    // String queryTrafoConstant =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/volumeError/transformationConstant";
  //    // Node trafoConstant = xmlServiceTools.getXPath_singleNode(
  //    // queryTrafoConstant, doc );
  //    // Node trafoConstValue = trafoConstant.getFirstChild();
  //    // String trafoConstantValue = trafoConstValue.getNodeValue();
  //    // trafoConstants[0] = Double.parseDouble( trafoConstantValue );
  //    // }
  //    // else
  //    // {
  //    // trafoConstants[0] = 0;
  //    // }
  //
  //    //Overall RMSE
  //    // String queryRMSE_mode =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError/@mode";
  //    // mode = xmlServiceTools.getXPath_singleNode( queryRMSE_mode, doc );
  //    // mValue = mode.getFirstChild();
  //    // modeValue = mValue.getNodeValue();
  //    // objectiveFunctions[1] = ( new Boolean( modeValue ) ).booleanValue();
  //    // if( !equalWeights && objectiveFunctions[1] )
  //    // {
  //    // String queryTrafoConstant =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError/transformationConstant";
  //    // Node trafoConstant = xmlServiceTools.getXPath_singleNode(
  //    // queryTrafoConstant, doc );
  //    // Node trafoConstValue = trafoConstant.getFirstChild();
  //    // String trafoConstantValue = trafoConstValue.getNodeValue();
  //    // trafoConstants[1] = Double.parseDouble( trafoConstantValue );
  //    // }
  //    // else
  //    // {
  //    // trafoConstants[1] = 0;
  //    // }
  //
  //    //Average RMSE peakFlows
  //    // String queryRMSEpeak_mode =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_PeakFlows/@mode";
  //    // mode = xmlServiceTools.getXPath_singleNode( queryRMSEpeak_mode, doc );
  //    // mValue = mode.getFirstChild();
  //    // modeValue = mValue.getNodeValue();
  //    // objectiveFunctions[2] = ( new Boolean( modeValue ) ).booleanValue();
  //    // if( objectiveFunctions[2] )
  //    // {
  //    // String queryPeakFlowLevel =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_PeakFlows/peakFlowLevel";
  //    // Node flowLevel = xmlServiceTools.getXPath_singleNode(
  // queryPeakFlowLevel,
  //    // doc );
  //    // Node flValue = flowLevel.getFirstChild();
  //    // String flowLevelValue = flValue.getNodeValue();
  //    // peakFlowLevel = Double.parseDouble( flowLevelValue );
  //    // }
  //    // if( !equalWeights && objectiveFunctions[2] )
  //    // {
  //    // String queryTrafoConstant =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_PeakFlows/transformationConstant";
  //    // Node trafoConstant = xmlServiceTools.getXPath_singleNode(
  //    // queryTrafoConstant, doc );
  //    // Node trafoConstValue = trafoConstant.getFirstChild();
  //    // String trafoConstantValue = trafoConstValue.getNodeValue();
  //    // trafoConstants[2] = Double.parseDouble( trafoConstantValue );
  //    // }
  //    // else
  //    // {
  //    // trafoConstants[2] = 0;
  //    // }
  //
  //    //Average RMSE lowFlows
  //    // String queryRMSElow_mode =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_LowFlows/@mode";
  //    // mode = xmlServiceTools.getXPath_singleNode( queryRMSElow_mode, doc );
  //    // mValue = mode.getFirstChild();
  //    // modeValue = mValue.getNodeValue();
  //    // objectiveFunctions[3] = ( new Boolean( modeValue ) ).booleanValue();
  //    // if( objectiveFunctions[3] )
  //    // {
  //    // String queryLowFlowLevel =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_LowFlows/lowFlowLevel";
  //    // Node flowLevel = xmlServiceTools.getXPath_singleNode( queryLowFlowLevel,
  //    // doc );
  //    // Node flValue = flowLevel.getFirstChild();
  //    // String flowLevelValue = flValue.getNodeValue();
  //    // lowFlowLevel = Double.parseDouble( flowLevelValue );
  //    // }
  //    // if( !equalWeights && objectiveFunctions[3] )
  //    // {
  //    // String queryTrafoConstant =
  //    //
  // "/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_LowFlows/transformationConstant";
  //    // Node trafoConstant = xmlServiceTools.getXPath_singleNode(
  //    // queryTrafoConstant, doc );
  //    // Node trafoConstValue = trafoConstant.getFirstChild();
  //    // String trafoConstantValue = trafoConstValue.getNodeValue();
  //    // trafoConstants[3] = Double.parseDouble( trafoConstantValue );
  //    // }
  //    // else
  //    // {
  //    // trafoConstants[3] = 0;
  //    // }
  //
  //    // System.out.println( "Objective function values: " );
  //    // System.out.println( "EqualWeights=" + equalWeights );
  //    // for( int i = 0; i < objectiveFunctions.length; i++ )
  //    // {
  //    // System.out.print( "Funktion " + i + ": " + objectiveFunctions[i] + ", "
  //    // );
  //    // System.out.println( "A=" + trafoConstants[i] );
  //    // }
  //    // System.out.println( "PeakFlowLevel=" + peakFlowLevel );
  //    // System.out.println( "LowFlowLevel=" + lowFlowLevel );
  //
  //    //xPath, ParamUpperBound, ParamLowerBound
  //    String queryID = "/autoCalibration/parameterlist/parameter/@ID";
  //    /*
  //     * String queryXPath = "/autoCalibration/parameterlist/parameter/xpath";
  //     */
  //    String queryUpBound =
  // "/autoCalibration/parameterlist/parameter/upperBound";
  //    String queryLoBound =
  // "/autoCalibration/parameterlist/parameter/lowerBound";
  //    String querySynValues =
  // "/autoCalibration/parameterlist/parameter/synteticValue";
  //    String queryInitialValues =
  // "/autoCalibration/parameterlist/parameter/initialValue";
  //    String queryFactor = "/autoCalibration/parameterlist/parameter/@mode";
  //
  //    NodeList nlID = XMLServiceTools.getXPath( queryID, doc );
  //    NodeList nlUpBound = XMLServiceTools.getXPath( queryUpBound, doc );
  //    NodeList nlLoBound = XMLServiceTools.getXPath( queryLoBound, doc );
  //    NodeList nlSynVal = XMLServiceTools.getXPath( querySynValues, doc );
  //    NodeList nlIniVal = XMLServiceTools.getXPath( queryInitialValues, doc );
  //    NodeList nlMode = XMLServiceTools.getXPath( queryFactor, doc );
  //
  //    int anzParam = nlID.getLength();
  //    System.out.println( "Anzahl Parameter: " + anzParam );
  //
  //    //xPaths = new String[anzParam];
  //    //paramUpperBounds = new double[anzParam];
  //    //paramLowerBounds = new double[anzParam];
  //    //modes = new String[anzParam];
  //    CalContext[] calContexts = new CalContext[anzParam];
  //
  //    for( int i = 0; i < anzParam; i++ )
  //    {
  //      // CalContext calContext = new CalContext();
  //      final Node nUB = ( nlUpBound.item( i ) ).getFirstChild();
  //      final double upperbound = Double.parseDouble( nUB.getNodeValue() );
  //      final Node nLB = ( nlLoBound.item( i ) ).getFirstChild();
  //      final double lowerBound = Double.parseDouble( nLB.getNodeValue() );
  //      final Node nI = ( nlIniVal.item( i ) ).getFirstChild();
  //      final double initial = Double.parseDouble( nI.getNodeValue() );
  //
  //      final Node nSyn = ( nlSynVal.item( i ) ).getFirstChild();
  //      final double synthetic = Double.parseDouble( nSyn.getNodeValue() );
  //
  //      final Node nM = nlMode.item( i );
  //      final String mode = nM.getNodeValue();
  //
  //      String id = ( nlID.item( i ) ).getNodeValue();
  //      String xPathquery = "/autoCalibration/parameterlist/parameter[@ID=\"" + id
  // + "\"]/xpath";
  //      NodeList nlXPath = XMLServiceTools.getXPath( xPathquery, doc );
  //      final String[] xPaths = new String[nlXPath.getLength()];
  //      for( int k = 0; k < nlXPath.getLength(); k++ )
  //      {
  //        xPaths[k] = ( ( nlXPath.item( k ) ).getFirstChild() ).getNodeValue();
  //      }
  //      calContexts[i] = new CalContext( initial, synthetic, lowerBound,
  // upperbound, mode, xPaths );
  //    }
  //    return calContexts;
  //  }
}


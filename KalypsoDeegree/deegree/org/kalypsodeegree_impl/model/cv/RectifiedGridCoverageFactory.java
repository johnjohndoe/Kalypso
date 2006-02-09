package org.kalypsodeegree_impl.model.cv;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Class for reading and writing RectifiedGridCoverages
 * 
 * @author N. Peiler
 */
public class RectifiedGridCoverageFactory
{

  private static String NSRGC = "http://elbe.wb.tu-harburg.de/rectifiedGridCoverage";

  /**
   * creates a RectifiedGridCoverage of a RectifiedGridCoverage-Feature
   * 
   * @param feat
   *          RectifiedGridCoverage-Feature
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridCoverage createRectifiedGridCoverage( Feature feat )
  {
    Feature feat_rectifiedGridCoverage = (Feature) feat.getProperty( "RectifiedGridCoverageMember" );
    RectifiedGridDomain rgDomain = (RectifiedGridDomain) feat_rectifiedGridCoverage.getProperty( "rectifiedGridDomain" );
    RangeSet rangeSet = (RangeSet) feat_rectifiedGridCoverage.getProperty("rangeSet" );
    RectifiedGridCoverage gridCoverage = new RectifiedGridCoverage( rgDomain, rangeSet );
    return gridCoverage;
  }

  /**
   * sets the data of a RectifiedGridCoverage in an gml-element
   * 
   * @param gridCoverage
   * @param element
   * @throws Exception
   */
  public static void writeRectifiedGridCoverage( RectifiedGridCoverage gridCoverage, Element element ) throws Exception
  {
    Document doc = element.getOwnerDocument();
    Element e_rectifiedGridCoverageMember = doc.createElement( "RectifiedGridCoverageMember" );
    Element e_rectifiedGridCoverage = doc.createElementNS( NSRGC, "rgc:RectifiedGridCoverage" );

    Element e_rectifiedGridDomain = doc.createElementNS( NSRGC, "rgc:rectifiedGridDomain" );
    RectifiedGridDomain gridDomain = gridCoverage.getGridDomain();
    IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler) MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( NSRGC, "RectifiedGridDomainType" ) );
    typeHandler.marshall( gridDomain, e_rectifiedGridDomain, null );
    e_rectifiedGridCoverage.appendChild( e_rectifiedGridDomain );

    Element e_rangeSet = doc.createElementNS( NSRGC, "rgc:rangeSet" );
    RangeSet rangeSet = gridCoverage.getRangeSet();
    typeHandler = (IMarshallingTypeHandler) MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( NSRGC, "RangeSetType" ) );
    typeHandler.marshall( rangeSet, e_rangeSet, null );
    e_rectifiedGridCoverage.appendChild( e_rangeSet );

    e_rectifiedGridCoverageMember.appendChild( e_rectifiedGridCoverage );
    element.appendChild( e_rectifiedGridCoverageMember );
  }
}
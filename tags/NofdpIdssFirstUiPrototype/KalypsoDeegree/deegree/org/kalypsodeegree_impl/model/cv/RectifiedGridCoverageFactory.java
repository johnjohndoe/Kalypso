package org.kalypsodeegree_impl.model.cv;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Class for reading and writing RectifiedGridCoverages
 * 
 * @author N. Peiler
 */
public class RectifiedGridCoverageFactory
{
  /**
   * creates a RectifiedGridCoverage of a RectifiedGridCoverage-Feature
   * 
   * @param feat
   *          RectifiedGridCoverage-Feature
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridCoverage createRectifiedGridCoverage( Feature feat )
  {
    // Gernots Remarks on Grids: TODO: directly work on the RectifiedGridCoverageMember - Feature
    // do not suppose its the root feature

    Feature feat_rectifiedGridCoverage = (Feature) feat.getProperty( "RectifiedGridCoverageMember" );

    // Gernots Remarks on Grids: TODO: this probably won't work, because the GridDomain is now parsed via the binding
    // types
    RectifiedGridDomain rgDomain = (RectifiedGridDomain) feat_rectifiedGridCoverage.getProperty( "rectifiedGridDomain" );

    RangeSet rangeSet = (RangeSet) feat_rectifiedGridCoverage.getProperty( "rangeSet" );
    RectifiedGridCoverage gridCoverage = new RectifiedGridCoverage( rgDomain, rangeSet );
    return gridCoverage;
  }

  // /**
  // * sets the data of a RectifiedGridCoverage in an gml-element
  // *
  // * @param gridCoverage
  // * @param element
  // * @throws Exception
  // */
  // public static void writeRectifiedGridCoverage( RectifiedGridCoverage gridCoverage, Element element ) throws
  // Exception
  // {
  // Document doc = element.getOwnerDocument();
  // Element e_rectifiedGridCoverageMember = doc.createElement( "RectifiedGridCoverageMember" );
  // Element e_rectifiedGridCoverage = doc.createElementNS( NSRGC, "rgc:RectifiedGridCoverage" );
  //
  // Element e_rectifiedGridDomain = doc.createElementNS( NSRGC, "rgc:rectifiedGridDomain" );
  // RectifiedGridDomain gridDomain = gridCoverage.getGridDomain();
  // IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler)
  // MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( NSRGC,
  // "RectifiedGridDomainType" ) );
  // typeHandler.marshall( gridDomain, e_rectifiedGridDomain, null );
  // e_rectifiedGridCoverage.appendChild( e_rectifiedGridDomain );
  //
  // Element e_rangeSet = doc.createElementNS( NSRGC, "rgc:rangeSet" );
  // RangeSet rangeSet = gridCoverage.getRangeSet();
  // typeHandler = (IMarshallingTypeHandler)
  // MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( NSRGC, "RangeSetType" ) );
  // typeHandler.marshall( rangeSet, e_rangeSet, null );
  // e_rectifiedGridCoverage.appendChild( e_rangeSet );
  //
  // e_rectifiedGridCoverageMember.appendChild( e_rectifiedGridCoverage );
  // element.appendChild( e_rectifiedGridCoverageMember );
  // }
}
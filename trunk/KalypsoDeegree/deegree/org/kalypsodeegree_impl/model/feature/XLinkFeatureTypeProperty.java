/**
 * TODO: license definieren
 */

/*
 * Created on Jun 26, 2004
 * 
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.deegree_impl.model.feature;

import org.deegree.model.feature.FeatureTypeProperty;

/**
 * this represents an enumeration featuretype property
 * 
 * @author doemming To change the template for this generated type comment go to
 *         Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class XLinkFeatureTypeProperty implements FeatureTypeProperty
{
  public final static int XLINK_UNKNOWN = -1;

  public final static int XLINK_SIMPLE = 1;

  public final static int XLINK_EXTENDED = 2;

  public final static int XLINK_LOCATOR = 3;

  public final static int XLINK_ARC = 4;

  public final static int XLINK_RESOURCE = 5;

  public final static int XLINK_TITLE = 6;

  public final static int ACTUATE_NOT_FIXED = 7;

  public final static int ACTUATE_ONLOAD = 8;

  public final static int ACTUATE_ONREQUEST = 9;

  public final static int USE_NOT_FIXED = 9;

  public final int myXLinkType;

  private final String myName;

  private final String myType;

  private final boolean myIsNullable;

  private int myActuate = ACTUATE_NOT_FIXED;

  private String myLabelFrom = null;

  private String myLabelTo = null;

  public XLinkFeatureTypeProperty( String name, int type, boolean isNullable )
  {
    myXLinkType = type;
    myName = name;
    myType = "java.lang.Object"; // TODO Type aus Schemadefinition lesen
    myIsNullable = isNullable;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.model.feature.FeatureTypeProperty#getName()
   */
  public String getName()
  {
    return myName;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.model.feature.FeatureTypeProperty#isNullable()
   */
  public boolean isNullable()
  {
    return myIsNullable;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.model.feature.FeatureTypeProperty#getType()
   */
  public final String getType()
  {
    return myType;
  }

  public final int getXLinkType()
  {
    return myXLinkType;
  }

  public final int getActuate()
  {
    return myActuate;
  }

  public void setActuate( int actuate )
  {
    myActuate = actuate;
  }

  public void setArc( String labelFrom, String labelTo )
  {
    myLabelFrom = labelFrom;
    myLabelTo = labelTo;
  }

  public String getLabelFrom()
  {
    return myLabelFrom;
  }

  public String getLabelTo()
  {
    return myLabelTo;
  }
  
  public String toString()
  {
      StringBuffer result=new StringBuffer("xlink:");
      switch(getXLinkType())
      {
        case XLINK_SIMPLE:
          result.append("simple");
          break;
      }
      return result.toString();
  }
}
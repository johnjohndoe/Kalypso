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

  public final int m_xlinkType;

  private final String m_name;
  private final String m_namespace;

  private final String m_type;

  private final boolean m_isNullable;

  private int myActuate = ACTUATE_NOT_FIXED;

  private String myLabelFrom = null;

  private String myLabelTo = null;

  public XLinkFeatureTypeProperty( String name,String namespace, int type, boolean isNullable)
  {
    m_xlinkType = type;
    m_name = name;
    m_namespace=namespace;
    m_type = "java.lang.Object"; // TODO Type aus Schemadefinition lesen
    m_isNullable = isNullable;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.model.feature.FeatureTypeProperty#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.model.feature.FeatureTypeProperty#isNullable()
   */
  public boolean isNullable()
  {
    return m_isNullable;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.model.feature.FeatureTypeProperty#getType()
   */
  public final String getType()
  {
    return m_type;
  }

  public final int getXLinkType()
  {
    return m_xlinkType;
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
    StringBuffer result = new StringBuffer( "xlink:" );
    switch( getXLinkType() )
    {
    case XLINK_SIMPLE:
      result.append( "simple" );
      break;
    }
    return result.toString();
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getNamespace()
   */
  public String getNamespace()
  {
    return m_namespace;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#isGeometryProperty()
   */
  public boolean isGeometryProperty()
  {
    return false;
  }
}
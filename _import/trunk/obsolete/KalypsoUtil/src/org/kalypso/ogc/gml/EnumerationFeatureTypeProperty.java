/** TODO: license definieren
*/

/*
 * Created on Jun 26, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.kalypso.ogc.gml;

import org.deegree.model.feature.FeatureTypeProperty;


/**
 *  this represents an enumeration featuretype property
 *
 * @author doemming To change the template for this generated type comment go to Window&gt;Preferences&gt;Java&gt;Code
 *         Generation&gt;Code and Comments
 */
public class EnumerationFeatureTypeProperty implements FeatureTypeProperty, Validator
{
    private String myName;
    private String myType;
    private Object[] myEnumeration;
    private boolean myIsNullable;

    public EnumerationFeatureTypeProperty( String name, String type, boolean isNullable, Object[] enumeration )
    {
        myName = name;
        myType = type;
        myIsNullable = isNullable;
        myEnumeration = enumeration;
    }

    public Object[] getEnumeration(  )
    {
        return myEnumeration;
    }

    /* (non-Javadoc)
     * @see org.deegree.model.feature.FeatureTypeProperty#getName()
     */
    public String getName(  )
    {
        return myName;
    }

    /* (non-Javadoc)
     * @see org.deegree.model.feature.FeatureTypeProperty#isNullable()
     */
    public boolean isNullable(  )
    {
        return myIsNullable;
    }

    /* (non-Javadoc)
     * @see org.deegree.model.feature.FeatureTypeProperty#getType()
     */
    public String getType(  )
    {
        return myType;
    }

    /* (non-Javadoc)
     * @see de.tuhh.wb.jm.schema.Validator#isValid(java.lang.Object)
     */
    public boolean isValid( Object object )
    {
        if( object == null && !myIsNullable )
            return false;

        for( int i = 0; i < myEnumeration.length; i++ )
        {
            if( myEnumeration.equals( object.toString(  ) ) )
                return true;
        }

        return false;
    }
}

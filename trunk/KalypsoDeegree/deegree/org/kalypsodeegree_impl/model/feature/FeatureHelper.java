package org.deegree_impl.model.feature;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.deegree.model.feature.Feature;

/**
 * @author doemming
 *  
 */
public class FeatureHelper
{
    public static boolean booleanIsTrue(Feature feature, String propName,
            boolean defaultStatus)
    {
        Object property = feature.getProperty(propName);
        if (property != null && property instanceof Boolean)
            return ((Boolean) property).booleanValue();
        return defaultStatus;
    }

    /**
     * @param fe
     * @param string
     * @param string2
     * @return
     */
    public static String getFormatedDate(Feature feature, String propName,
            String simpleDateFormatPattern, String defaultValue)
    {
        Object property = feature.getProperty(propName);
        if (property != null && property instanceof Date)
        {
            DateFormat dateFormat = new SimpleDateFormat(
                    simpleDateFormatPattern);
            return dateFormat.format((Date)property);
        }
        return defaultValue;

    }
}
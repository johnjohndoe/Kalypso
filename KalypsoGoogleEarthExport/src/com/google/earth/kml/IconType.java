//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.0-b26-ea3 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.09.30 at 03:28:54 PM CEST 
//


package com.google.earth.kml;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.google.earth.kml.IconType;
import com.google.earth.kml.LinkType;


/**
 * <p>Java class for IconType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="IconType">
 *   &lt;complexContent>
 *     &lt;extension base="{http://earth.google.com/kml/2.1}LinkType">
 *       &lt;sequence>
 *         &lt;element name="x" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="y" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="w" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="h" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "IconType", propOrder = {
    "x",
    "y",
    "w",
    "h"
})
public class IconType
    extends LinkType
{

    @XmlElement(namespace = "http://earth.google.com/kml/2.1", type = Integer.class)
    protected int x;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", type = Integer.class)
    protected int y;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", type = Integer.class)
    protected int w;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", type = Integer.class)
    protected int h;

    /**
     * Gets the value of the x property.
     * 
     */
    public int getX() {
        return x;
    }

    /**
     * Sets the value of the x property.
     * 
     */
    public void setX(int value) {
        this.x = value;
    }

    /**
     * Gets the value of the y property.
     * 
     */
    public int getY() {
        return y;
    }

    /**
     * Sets the value of the y property.
     * 
     */
    public void setY(int value) {
        this.y = value;
    }

    /**
     * Gets the value of the w property.
     * 
     */
    public int getW() {
        return w;
    }

    /**
     * Sets the value of the w property.
     * 
     */
    public void setW(int value) {
        this.w = value;
    }

    /**
     * Gets the value of the h property.
     * 
     */
    public int getH() {
        return h;
    }

    /**
     * Sets the value of the h property.
     * 
     */
    public void setH(int value) {
        this.h = value;
    }

}

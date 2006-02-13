
package org.kalypso.services.calculation.service.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.calculation.service.impl.jaxws.GetJob;

@XmlRootElement(name = "getJob", namespace = "http://impl.service.calculation.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getJob", namespace = "http://impl.service.calculation.services.kalypso.org/")
public class GetJob {

    @XmlElement(name = "arg0", namespace = "")
    private String arg0;

    /**
     * 
     * @return
     *     returns String
     */
    public String getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(String arg0) {
        this.arg0 = arg0;
    }

}

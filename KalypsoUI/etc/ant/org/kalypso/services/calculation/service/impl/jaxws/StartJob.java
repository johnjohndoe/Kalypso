
package org.kalypso.services.calculation.service.impl.jaxws;

import javax.activation.DataHandler;
import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.calculation.service.impl.jaxws.StartJob;

@XmlRootElement(name = "startJob", namespace = "http://impl.service.calculation.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "startJob", namespace = "http://impl.service.calculation.services.kalypso.org/", propOrder = {
    "arg0",
    "arg1",
    "arg2",
    "arg3",
    "arg4"
})
public class StartJob {

    @XmlElement(name = "arg0", namespace = "")
    private String arg0;
    @XmlElement(name = "arg1", namespace = "")
    private String arg1;
    @XmlElement(name = "arg2", namespace = "")
    private DataHandler arg2;
    @XmlElement(name = "arg3", namespace = "")
    private org.kalypso.services.calculation.service.CalcJobClientBean[] arg3;
    @XmlElement(name = "arg4", namespace = "")
    private org.kalypso.services.calculation.service.CalcJobClientBean[] arg4;

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

    /**
     * 
     * @return
     *     returns String
     */
    public String getArg1() {
        return this.arg1;
    }

    /**
     * 
     * @param arg1
     *     the value for the arg1 property
     */
    public void setArg1(String arg1) {
        this.arg1 = arg1;
    }

    /**
     * 
     * @return
     *     returns DataHandler
     */
    public DataHandler getArg2() {
        return this.arg2;
    }

    /**
     * 
     * @param arg2
     *     the value for the arg2 property
     */
    public void setArg2(DataHandler arg2) {
        this.arg2 = arg2;
    }

    /**
     * 
     * @return
     *     returns CalcJobClientBean[]
     */
    public org.kalypso.services.calculation.service.CalcJobClientBean[] getArg3() {
        return this.arg3;
    }

    /**
     * 
     * @param arg3
     *     the value for the arg3 property
     */
    public void setArg3(org.kalypso.services.calculation.service.CalcJobClientBean[] arg3) {
        this.arg3 = arg3;
    }

    /**
     * 
     * @return
     *     returns CalcJobClientBean[]
     */
    public org.kalypso.services.calculation.service.CalcJobClientBean[] getArg4() {
        return this.arg4;
    }

    /**
     * 
     * @param arg4
     *     the value for the arg4 property
     */
    public void setArg4(org.kalypso.services.calculation.service.CalcJobClientBean[] arg4) {
        this.arg4 = arg4;
    }

}

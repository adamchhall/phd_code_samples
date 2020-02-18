# --------------------------------------------------------------------------------------------
# UNIFIED PRICE INDEX CALCULATION FUNCTIONS
# --------------------------------------------------------------------------------------------

# Import the helper functions on which these functions depend
source('~/Scripts/helper_functions.r')

# Function to calculate the Unified Price Index (UPI) from Redding Weinstein (2017)
# DEPENDS: gmean, prep_upi_data
calc_upi<-function(eli_data, base, comp, eos=10, myyr=2009, mod=NA, 
                   type="stfips", sharevar="exp_share", pricevar="quant_price", modvar="product_module_code", 
                   aggfun=weighted.mean, return.components=FALSE, verbose=FALSE) {
  
  if(verbose) {
    # Start timer on function execution
    start_time<-Sys.time()
  }
  
  # Omit any unnecessary data to compute the bilateral comparison
  upi_data<-eval(parse(text=paste0("subset(eli_data, ", type, "%in%c(base,comp))")))
  
  # Subset to one product module, if requested.
  # Otherwise, compute across all product modules.
  if(!is.na(mod) & modvar%in%colnames(upi_data)) {
    eval(parse(text=paste0("upi_data <- upi_data[upi_data$", modvar, "==mod,]")))
    #upi_data<-upi_data[upi_data[,modvar]==mod,]
  }
  
  # Check to make sure there is still data left after subsetting
  if(nrow(upi_data)==0) {
    if(return.components) {
      return(data.frame(base=base, comp=comp, upi=NA, cgpi=NA, sadj=NA, vadj=NA, 
                        lambda_comp=NA, lambda_base=NA, n_common=NA, 
                        product_module_code=mod, eos=eos))
    } else {
      return(NA)
    }
  }
  
  # Check to make sure that there's a unique price per UPC x area
  eval(parse(text=paste0("base_upcs_duplicated <- length(unique(upi_data[upi_data$", type, "==base,]$upc)) != length(upi_data[upi_data$", type, "==base,]$upc)")))
  eval(parse(text=paste0("comp_upcs_duplicated <- length(unique(upi_data[upi_data$", type, "==comp,]$upc)) != length(upi_data[upi_data$", type, "==comp,]$upc)")))
  if(base_upcs_duplicated | comp_upcs_duplicated) {
    stop("Input data does not appear to have one price per upc x area.")
  }
  
  # Find UPCs that are shared between both areas
  shared_upcs<-eval(parse(text=paste0("intersect(unique(upi_data$upc[upi_data$", type, "==base]), 
                                      unique(upi_data$upc[upi_data$", type, "==comp]))")))
  
  # Return NA if there is no overlap between the areas.
  if(length(shared_upcs)==0) {
    if(verbose) {print("No product varieties in-common between areas in input data. Returning NA.", quote=FALSE)}
    if(return.components) {
      return(data.frame(base=base, comp=comp, upi=NA, cgpi=NA, sadj=NA, vadj=NA, 
                        lambda_comp=NA, lambda_base=NA, n_common=0, product_module_code=mod, eos=eos))
    } else {
      return(NA)
    }
  }
  
  # Find the data for the goods common to both areas
  upi_common<-upi_data[upi_data$upc%in%shared_upcs,]
  upi_common<-eval(parse(text=paste0("upi_common[order(upi_common$", type, ", upi_common$upc),]")))
  
  # Find the common prices and expenditure shares in each area
  base_com_shr<-eval(parse(text=paste0("upi_common$", sharevar, "[upi_common$", type, "==base] / sum(upi_common$", sharevar, "[upi_common$", type, "==base])")))
  comp_com_shr<-eval(parse(text=paste0("upi_common$", sharevar, "[upi_common$", type, "==comp] / sum(upi_common$", sharevar, "[upi_common$", type, "==comp])")))
  base_com_prices<-eval(parse(text=paste0("upi_common$", pricevar, "[upi_common$", type, "==base]")))
  comp_com_prices<-eval(parse(text=paste0("upi_common$", pricevar, "[upi_common$", type, "==comp]")))
  
  # Compute the common goods price index
  cgpi<-gmean(base_com_prices/comp_com_prices)
  
  # Compute the share adjustment term  
  sadj<-(gmean(comp_com_shr/base_com_shr))^(1/(eos-1))
  
  # Find the lambda terms
  lambda_base<-eval(parse(text=paste0("sum(upi_data$", sharevar, "[upi_data$", type, "==base & upi_data$upc%in%shared_upcs])")))
  lambda_comp<-eval(parse(text=paste0("sum(upi_data$", sharevar, "[upi_data$", type, "==comp & upi_data$upc%in%shared_upcs])")))
  
  # Compute the variety adjustment
  vadj<-(lambda_comp / lambda_base)^(1/(eos-1))
  
  if(verbose) {
    # End timer on function execution
    end_time<-Sys.time()
    print("Total execution time: ", quote=FALSE)
    print(difftime(end_time, start_time, units="auto"))
  }
  
  # Output index value
  if(return.components==TRUE) {
    return(data.frame(base=base, comp=comp, upi=cgpi*sadj*vadj, cgpi=cgpi, sadj=sadj, vadj=vadj, 
                      lambda_comp=lambda_comp, lambda_base=lambda_base, n_common=length(shared_upcs), 
                      product_module_code=mod, eos=eos))
  } else {
    return(cgpi*sadj*vadj)
  }
  
}

calc_upi_matrix<-function(eli_data, agglvl="stfips", mod=NA, eos=10, myyr=2009, verbose=FALSE,
                          sharevar="exp_share", pricevar="quant_price", modvar="product_module_code", 
                          aggfn=weighted.mean) {
  
  # Import dependencies to run faster
  require(RcppAlgos)
  require(data.table)
  
  # Start timer on function execution
  if(verbose) {
    start_time<-Sys.time()  
  }

  # Convert input to data.table and filter out rows from wrong product modules
  eli_data <- as.data.table(eli_data)
    
  # Remove unnecessary data
  if(is.na(mod)) {
    eli_data <- eli_data[year==myyr]  
  } else {
    eli_data <- eli_data[year==myyr & product_module_code==mod]
  }
  
  # Avoid weird errors by converting to string
  eval(parse(text=paste0("eli_data$", agglvl, " <- as.character(eli_data$", agglvl, ")")))
  
  # Find all the areas included in the input data
  areas<-eval(parse(text=paste0("unique(eli_data$", agglvl, ")")))
  
  # Throw an error if there aren't any areas in the data to compare.
  if(length(areas)==0) {
    stop("No areas in input data.")
  }
  
  # Handle case where there is only one county in the data
  if(length(areas)==1) {
    warning("No comparisons can be made because there is only one county in the supplied input data.")
    return(data.frame(base=areas, comp=areas, upi=1, cgpi=1, sadj=1, vadj=1, lambda_comp=1, lambda_base=1, 
                      n_common=length(unique(eli_data$upc)), product_module_code=mod, eos=eos))
  }
  
  # Calculate all possible bilateral comparisons
  datarows<-comboGeneral(areas, 2, FUN=function(area_vec) {
    calc_upi(eli_data=eli_data, base=area_vec[1], comp=area_vec[2], eos=eos, myyr=myyr, mod=mod,
             type=agglvl, sharevar=sharevar, pricevar=pricevar, modvar=modvar, aggfun=aggfn, return.components=TRUE)
  })
  output<-rbindlist(datarows)
  
  # Compute reverse comparisons
  output<-rbind(output, data.frame(base=output$comp, comp=output$base, upi=1/output$upi, 
                                   cgpi=1/output$cgpi, sadj=1/output$sadj, vadj=1/output$vadj,
                                   lambda_comp=output$lambda_base, lambda_base=output$lambda_comp, 
                                   n_common=output$n_common, product_module_code=mod, eos=eos))
  
  # Add in self comparisons
  self_n_common <- eval(parse(text=paste0("eli_data[order(", agglvl, "), .N, by=list(", agglvl, ")]")))
  self_n_common_names <- eval(parse(text=paste0("self_n_common$", agglvl)))
  output<-rbind(output, data.frame(base=self_n_common_names, comp=self_n_common_names, 
                                   upi=1, cgpi=1, sadj=1, vadj=1, lambda_comp=1, lambda_base=1, 
                                   n_common=self_n_common$N,
                                   product_module_code=mod, eos=eos))
  
  # Sort the output by base and comparison areas
  output$base<-as.character(output$base)
  output$comp<-as.character(output$comp)
  output<-output[order(output$base, output$comp),]
  
  if(verbose) {
    end_time<-Sys.time()
    print("Total execution time: ", quote=FALSE)
    print(difftime(end_time, start_time, units="mins"))
  }
  
  return(output)
}

# --------------------------------------------------------------------------------------------
# TRANSITIVIZATION FUNCTIONS
# --------------------------------------------------------------------------------------------

# Calculates a multilateral index which is the result of applying the
# EKS method to the bilateral indices in fmtx. This also results in imputations
# based on a linear model for all of the non-comparable areas not included in fmtx.
calc_wgeks<-function(fmtx, wvar=NULL, idxvar='upi', ref.area=1, return.pi=FALSE, return.model=FALSE, return.se=FALSE) {
  
  # Convert input data to a data frame object just in case
  fmtx<-as.data.frame(fmtx)
  
  # Convert base / comp to character for easier comparison
  fmtx$base<-as.character(fmtx$base)
  fmtx$comp<-as.character(fmtx$comp)
  
  # Exclude self comparisons (with index values of 1)
  fmtx_self<-fmtx[fmtx$base==fmtx$comp,]
  fmtx<-fmtx[fmtx$base!=fmtx$comp,]
  
  # If there is only one area, return the trivial result
  if(nrow(fmtx)==0) {
    
    if(return.model) {
      stop("Error: There is only one county in the input data. No model can be fit.")
    } else {
      # Throw a warning
      warning("No comparisons can be made because there is only one county in the supplied input data.")
      output<-data.frame(fmtx_self, weight=1, trans_epi=1)
      colnames(output)[colnames(output)=='trans_epi']<-paste0('trans_', idxvar)
      
      # If se is requested, set se to one (exp(0)=1)
      if(return.se) {
        output$trans_epi_gse<-1
        colnames(output)[colnames(output)=='trans_epi_gse']<-paste0('trans_', idxvar, '_gse')
      }
      
      # If pi is requested, set all pis to zero
      if(return.pi) {
        output$comp_pi<-0
        output$base_pi<-0
      }
      
      # return the trivial output
      return(output)
    }
  }
  
  # Get rid of any rows of the input data comparing areas for which all indices are NA.
  # This prevents there from being a rank-deficient fit problem where some
  # of the estimated pi's become NA because after removing NA's, there are
  # no data on some of the input areas.
  fmtx_areas<-unique(c(fmtx[!is.na(fmtx[,idxvar]), 'base'], fmtx[!is.na(fmtx[,idxvar]), 'comp']))
  fmtx<-fmtx[fmtx$base%in%fmtx_areas & fmtx$comp%in%fmtx_areas,]
  fmtx_self<-fmtx_self[fmtx_self$base%in%fmtx_areas,]
  
  # Find the number of areas remaining
  areas<-sort(unique(fmtx$base))
  n_areas<-length(areas)

  if(!is.null(wvar)) {
    colnames(fmtx)[which(colnames(fmtx)==wvar)]<-"weight"
    colnames(fmtx_self)[which(colnames(fmtx_self)==wvar)]<-"weight"
    #colnames(fmtx)[which(!(colnames(fmtx)%in%c("base", "comp", "weight")))]<-"epi"
  } else {
    fmtx$weight<-1
  }
    
  # Attempt a workaround for huge integers...
  n_row<-as.numeric(as.character(nrow(fmtx)))
  
  # Initialize empty matrix
  glm_data<-matrix(rep(0,n_areas*n_row), nrow=n_row)
  
  for(i in 1:nrow(fmtx)) {
    # Make indicator for base and comparison area in X matrix
    glm_data[i,]<-areas%in%fmtx[i,"comp"] - areas%in%fmtx[i,"base"]
  }
  
  # Add the comparisons
  glm_data<-as.data.frame(glm_data)
  glm_data$epi<-fmtx[,idxvar]
  
  # Remove the area chosen as the numeraire from the tableau (?)
  colnames(glm_data)<-c(areas, "epi")
  glm_data<-glm_data[,-ref.area]
  
  model<-lm(log(epi)~0+., weights=fmtx$weight, na.action=na.omit, data=glm_data)
  
  if(return.model) {
    return(model)
  } else {
    output<-data.frame(fmtx, trans_epi=exp(predict(model, newdata=glm_data)))
    
    # Add gse to data if requested
    if(return.se) {
      trans_preds<-predict(model, newdata=glm_data, se.fit=TRUE)
      output$trans_epi_gse<-exp(trans_preds$se.fit)
      colnames(output)[colnames(output)=='trans_epi_gse']<-paste0('trans_', idxvar, '_gse')
    }
    
    # Add self comparisons back into the data if included in input data
    if(nrow(fmtx_self)>0) {
      self_comp<-data.frame(fmtx_self, trans_epi=1)  
      self_comp$weight <- 0
      
      # Attached gse if requested
      if(return.se) {
        self_comp$trans_epi_gse=1
        colnames(self_comp)[colnames(self_comp)=='trans_epi_gse']<-paste0('trans_', idxvar, '_gse')
      }
      
      # Attach self comparisons to the data
      output<-rbind(output, self_comp)
    }
    
    # Add pi values to data frame if requested
    if(return.pi) {
      pidat<-data.frame(areas[-ref.area], data.frame(coef(model)))
      colnames(pidat)<-c("comp", "comp_pi")
      output<-merge(output, pidat, by="comp", all.x=TRUE)
      colnames(pidat)<-c("base", "base_pi")
      output<-merge(output, pidat, by="base", all.x=TRUE)
      output$comp_pi[is.na(output$comp_pi)]<-0
      output$base_pi[is.na(output$base_pi)]<-0
    }
    
    # Order the output by base and comparison area
    output<-output[order(output$base, output$comp),]
    colnames(output)[colnames(output)=='trans_epi']<-paste0('trans_', idxvar)
    
    return(output)
    
  }
  
}
